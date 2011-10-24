#include "erl_nif.h"

ErlNifResourceType* bsn_type;
ERL_NIF_TERM ATOM_TRUE, ATOM_FALSE;

/*
typedef struct {
    unsigned size;
    unsigned char* data;
} ErlNifBinary;

*/

struct bsn_elem_struct {
	ErlNifBinary bin;
	struct bsn_elem_struct* next;
};
typedef struct bsn_elem_struct bsn_elem;

typedef bsn_elem* bsn_list;

typedef struct {
	unsigned int count; /* count of elements */
	unsigned int max; 	/* count of slots */
	ErlNifMutex *mutex;
	bsn_list* list;
} bsn_res;


inline static ERL_NIF_TERM bool_to_term(int value) {
    return value ? ATOM_TRUE : ATOM_FALSE; 
}

/* Calculate the sum of chars. */
unsigned int 
private_hash(const ErlNifBinary* b, unsigned int max)
{
	unsigned char* ptr;
	unsigned int i, sum = 0;

	ptr = b->data;
	i   = b->size;

	for (; i; i--, ptr++)
		sum += *ptr;

	return sum % max;
}

inline void 
private_clear_elem(bsn_elem* el)
{
	enif_release_binary(&(el->bin));
	enif_free(el);
}

inline void 
private_chain_clear_all(bsn_elem* ptr)
{
	bsn_elem* next;

	while (ptr != NULL) {
		
		next = ptr->next;
		private_clear_elem(ptr);	
		ptr = next;
	}
}

inline int 
private_compare(ErlNifBinary* b1, ErlNifBinary* b2)
{
	unsigned char* p1; 
	unsigned char* p2;
	unsigned len;

	if (b1->size != b2->size)
		return 0;
	
	p1 = b1->data;
	p2 = b2->data;
	len = b1->size;
	
	while (len) {
    	if ((*p1) != (*p2))
    		return 0;

		len--; p1++; p2++;
	}
	return 1;
}

/* Skip existing elements. If the element bin is not found, return last element. 
 * If el.bin == bin, return el. */
bsn_elem* 
private_chain_shift(bsn_elem* ptr, ErlNifBinary* bin, int* num_ptr)
{
	(*num_ptr)++;
	if ((ptr) == NULL)
		return ptr;

	while (1) {
		if (private_compare(&(ptr->bin), bin)) {
			/* found an equal binary. Invert num */
			(*num_ptr) *= -1;
			return ptr;
		}
		if ((ptr->next) == NULL)
			return ptr;
		ptr = ptr->next;
		(*num_ptr)++;
	}
}

/* Append the element `el' to the chain `chain' */
void 
private_chain_append(bsn_elem** chain, bsn_elem* el, int* num_ptr)
{
	bsn_elem* last;

	if ((*chain) == NULL) {
		/* The new element is last */
		*chain = el;
	} else {
		last = private_chain_shift(*chain, &(el->bin), num_ptr);
		if ((*num_ptr) < 0) {
			/* Element was already added. */
			private_clear_elem(el);
		} else {
			last->next = el;
		}
	}
}

bsn_elem*
private_chain_shift_clear(bsn_elem** ptr, ErlNifBinary* bin, int* num_ptr)
{
	bsn_elem** prev = NULL;
	bsn_elem* el;

	while ((*ptr) != NULL) {
		if (private_compare(&((*ptr)->bin), bin)) {
			(*num_ptr) *= -1;

			/* found an equal binary. Delete elem. Invert num */
			if (prev == NULL) {
				el = *ptr;
				(*ptr) = (*ptr)->next;
				return el;
			}
			*prev = (*ptr)->next;
			return *ptr;
		}
		prev = ptr;
		el = *ptr;
		ptr = (bsn_elem**) &(el->next);
		(*num_ptr)++;
	}

	return NULL;
}

static ERL_NIF_TERM 
bsn_new(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	unsigned int max;
	bsn_list* ptr;
	bsn_res* r;

	if (!(enif_get_uint(env, argv[0], &max) && (max>0)))
		return enif_make_badarg(env);

	ptr = enif_alloc(sizeof(bsn_list) * max);
	if (ptr == NULL)
		return enif_make_badarg(env);

	r = (bsn_res*) enif_alloc_resource(bsn_type, sizeof(bsn_res));
	r->mutex = enif_mutex_create("Mutex for the BSN writer");
	r->count = 0;
	r->max = max;
	r->list = ptr;

	for (; max; max--, ptr++)
		*ptr = NULL;

	return enif_make_resource(env, r);
}

static ERL_NIF_TERM 
bsn_add(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	ErlNifBinary bin;
	bsn_res* r;
	unsigned int pos;
	int num = 0;
	bsn_elem* elem_ptr;
	
	if (!(enif_get_resource(env, argv[0], bsn_type, (void**) &r)
		&& enif_inspect_binary(env, argv[1], &bin)))
		return enif_make_badarg(env);

	enif_realloc_binary(&bin, bin.size);
	pos = private_hash(&bin, r->max);

	elem_ptr = enif_alloc(sizeof(bsn_elem));
	if (elem_ptr == NULL)
		return enif_make_badarg(env);

	elem_ptr->next = NULL;
	elem_ptr->bin = bin;

	enif_mutex_lock(r->mutex);
	private_chain_append(&(r->list[pos]), elem_ptr, &num);
	if (num >= 0) 
		(r->count)++;
	enif_mutex_unlock(r->mutex);
	
	/* Already added */
	if (num < 0) 
		enif_release_binary(&(bin));

	return enif_make_int(env, num);
}

static ERL_NIF_TERM 
bsn_search(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	ErlNifBinary bin;
	bsn_res* r;
	unsigned int pos;
	int num = 0;
	
	if (!(enif_get_resource(env, argv[0], bsn_type, (void**) &r)
		&& enif_inspect_binary(env, argv[1], &bin)))
		return enif_make_badarg(env);

	pos = private_hash(&bin, r->max);

	enif_mutex_lock(r->mutex);
	private_chain_shift(r->list[pos], &bin, &num);
	enif_mutex_unlock(r->mutex);
	
	return enif_make_int(env, num);
}

static ERL_NIF_TERM 
bsn_clear(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	ErlNifBinary bin;
	bsn_res* r;
	unsigned int pos;
	int num = 0;
	bsn_elem* elem_ptr;
	
	if (!(enif_get_resource(env, argv[0], bsn_type, (void**) &r)
		&& enif_inspect_binary(env, argv[1], &bin)))
		return enif_make_badarg(env);

	pos = private_hash(&bin, r->max);

	enif_mutex_lock(r->mutex);
	elem_ptr = private_chain_shift_clear(&(r->list[pos]), &bin, &num);
	if (elem_ptr != NULL) {
		private_clear_elem(elem_ptr);
		(r->count)--;
	}
	enif_mutex_unlock(r->mutex);
	
	return enif_make_int(env, num);
}

static ERL_NIF_TERM 
bsn_all_chain(ErlNifEnv* env, bsn_elem* e, ERL_NIF_TERM tail)
{
	ERL_NIF_TERM head;
	ErlNifBinary bin;
	while (e != NULL) {
		bin = e->bin;
		enif_realloc_binary(&bin, bin.size);
		head = enif_make_binary(env, &bin);
		tail = enif_make_list_cell(env, head, tail);
		e = e->next;
	}
	return tail;
}
  
static ERL_NIF_TERM 
bsn_chains(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	bsn_res* r;
	unsigned int max;
	bsn_list* ptr;
	ERL_NIF_TERM tail, head;
	
	if (!enif_get_resource(env, argv[0], bsn_type, (void**) &r))
		return enif_make_badarg(env);
	tail = enif_make_list(env, 0);

	ptr = r->list;

	enif_mutex_lock(r->mutex);
	max = r->max;

	while (max) {
		head = enif_make_list(env, 0);
		head = bsn_all_chain(env, *ptr, head);
		tail = enif_make_list_cell(env, head, tail);
		
		ptr++;
		max--;
	}
	enif_mutex_unlock(r->mutex);
	
	return tail;
}
  
static ERL_NIF_TERM 
bsn_all(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	bsn_res* r;
	unsigned int max;
	bsn_list* ptr;
	ERL_NIF_TERM list;
	
	if (!enif_get_resource(env, argv[0], bsn_type, (void**) &r))
		return enif_make_badarg(env);
	list = enif_make_list(env, 0);

	ptr = r->list;

	enif_mutex_lock(r->mutex);
	max = r->max;

	while (max) {
		list = bsn_all_chain(env, *ptr, list);
		ptr++;
		max--;
	}
	enif_mutex_unlock(r->mutex);
	
	return list;
}
  

static ERL_NIF_TERM 
bsn_count(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	bsn_res* r;
	
	if (!enif_get_resource(env, argv[0], bsn_type, (void**) &r))
		return enif_make_badarg(env);

	return enif_make_int(env, r->count);
}


static ERL_NIF_TERM 
bsn_hash(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	ErlNifBinary bin;
	unsigned int max;

	if (!(enif_inspect_binary(env, argv[0], &bin)
		&& enif_get_uint(env, argv[1], &max) && (max>0)))
		return enif_make_badarg(env);

	return enif_make_uint(env,
		private_hash(&bin, max));
}


static ERL_NIF_TERM 
bsn_compare(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	ErlNifBinary b1, b2;

	if (!(enif_inspect_binary(env, argv[0], &b1)
		&& enif_inspect_binary(env, argv[1], &b2)))
		return enif_make_badarg(env);

	return bool_to_term(private_compare(&b1, &b2));
}

void private_clear_all(bsn_res* r)
{
	unsigned int max;
	bsn_list* ptr;
	max = r->max;
	ptr = r->list;

	while (max) {
		private_chain_clear_all(*ptr);
		ptr++;
		max--;
	}
}

void 
bsn_type_dtor(ErlNifEnv* env, void* obj) 
{
	bsn_res* r = (bsn_res*) obj;
	private_clear_all(r);
	enif_mutex_destroy(r->mutex);
	enif_free(r->list);
}



int
on_load(ErlNifEnv* env, void** priv, ERL_NIF_TERM info)
{
    ATOM_TRUE     = enif_make_atom(env, "true");
    ATOM_FALSE    = enif_make_atom(env, "false");
	
    ErlNifResourceFlags flags = (ErlNifResourceFlags)(ERL_NIF_RT_CREATE |
        ERL_NIF_RT_TAKEOVER);

    bsn_type = enif_open_resource_type(env, NULL, "bsn_type",
        bsn_type_dtor, flags, NULL); 

    if (bsn_type == NULL) return 1;

    return 0;
}


int
on_upgrade(ErlNifEnv* env, void** priv, void** old_priv, ERL_NIF_TERM info)
{
    return 0;
}


static ErlNifFunc nif_functions[] = {
    {"new",     1, bsn_new},
    {"add",     2, bsn_add},
    {"all",     1, bsn_all},
    {"chains",  1, bsn_chains},
    {"in",      2, bsn_search},
    {"clear",   2, bsn_clear},
    {"count",   1, bsn_count},

    {"hash",    2, bsn_hash},
    {"compare", 2, bsn_compare},
};


ERL_NIF_INIT(bsn_ext, nif_functions, &on_load, &on_load, &on_upgrade, NULL);
