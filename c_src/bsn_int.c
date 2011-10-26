#include "erl_nif.h"


ErlNifResourceType* bsn_type;
ERL_NIF_TERM ATOM_TRUE, ATOM_FALSE, ATOM_NO_MORE;

struct bsn_elem_struct {
	ErlNifBinary bin;
	unsigned int hash;
};
typedef struct bsn_elem_struct bsn_elem;


typedef struct {
	unsigned int count; /* count of elements */
	unsigned int max; 	/* count of slots */
	ErlNifMutex *mutex;
	bsn_elem* list;
	unsigned int (*next_pos)
		(void*, unsigned int, unsigned int);
} bsn_res;


inline static ERL_NIF_TERM bool_to_term(int value) {
    return value ? ATOM_TRUE : ATOM_FALSE; 
}

unsigned int next_pos_linear(bsn_res* r, unsigned int hash, unsigned int step) {
	return (hash + step) % (r->max);
}

unsigned int next_pos_quadric(bsn_res* r, unsigned int hash, unsigned int step) {
	return (hash + (step*step)) % (r->max);
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


static ERL_NIF_TERM 
bsn_new(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	int max; /* This value will be set by a client: 
				if (max<0) -> use quadric algorithm */
	bsn_elem* ptr;
	bsn_res* r;

	if (!enif_get_int(env, argv[0], &max) || (max == 0))
		return enif_make_badarg(env);


	r = (bsn_res*) enif_alloc_resource(bsn_type, sizeof(bsn_res));
	r->mutex = enif_mutex_create("Mutex for the BSN writer");
	r->count = 0;

	/* Select an algorithm */
	if (max>0) {
		r->next_pos = &next_pos_linear;
	} else if (max<0) {
		r->next_pos = &next_pos_quadric;
		max *= -1; 
	} 
	/* Now max is cells' count in the array. */
	r->max = (unsigned int) max;

	ptr = enif_alloc(sizeof(bsn_elem) * max);
	if (ptr == NULL)
		return enif_make_badarg(env);
	r->list = ptr;

	for (; max; max--, ptr++)
		ptr->hash = r->max;


	return enif_make_resource(env, r);
}

static ERL_NIF_TERM 
bsn_add(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	ErlNifBinary bin;
	bsn_res* r;
	unsigned int pos, hash, max;
	int num = 0;
	bsn_elem* elem_ptr;
	
	if (!(enif_get_resource(env, argv[0], bsn_type, (void**) &r)
		&& enif_inspect_binary(env, argv[1], &bin)))
		return enif_make_badarg(env);

	enif_realloc_binary(&bin, bin.size);
	hash = pos = private_hash(&bin, r->max);


	enif_mutex_lock(r->mutex);
	max = r->max;

	while (num < max) {
		elem_ptr = &(r->list[pos]);
		/* Found free space */
		if (elem_ptr->hash == max) {
			elem_ptr->bin = bin;
			elem_ptr->hash = hash;
			break;
		}


		/* Found elem */
		if ((elem_ptr->hash == hash)
			&& private_compare(&bin, &(elem_ptr->bin))) {
			num *= -1;
			break;
		}

		pos = (r->next_pos)(r, hash, num);
		num++;
	}
	if ((num >= 0) && (num < max))
		(r->count)++;

	enif_mutex_unlock(r->mutex);

	/* Error: already added or owerflow */
	if (!((num >= 0) && (num < max)))
		enif_release_binary(&bin);

	if (num >= max)
		return ATOM_NO_MORE;
	
	return enif_make_int(env, num);
}

static ERL_NIF_TERM 
bsn_search(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	ErlNifBinary bin;
	bsn_res* r;
	unsigned int pos, max, hash;
	int num = 1;
	bsn_elem* elem_ptr;
	
	if (!(enif_get_resource(env, argv[0], bsn_type, (void**) &r)
		&& enif_inspect_binary(env, argv[1], &bin)))
		return enif_make_badarg(env);

	hash = pos = private_hash(&bin, r->max);

	enif_mutex_lock(r->mutex);
	max = r->max;

	while (num < max) {
		elem_ptr = &(r->list[pos]);
		/* Found free space */
		if (elem_ptr->hash == max) {
			break;
		}


		/* Found elem */
		if ((elem_ptr->hash == hash)
			&& private_compare(&bin, &(elem_ptr->bin))) {
			num *= -1;
			break;
		}

		pos = (r->next_pos)(r, hash, num);
		num++;
	}
	enif_mutex_unlock(r->mutex);
	
	return enif_make_int(env, num);
}

static ERL_NIF_TERM 
bsn_clear(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	return enif_make_badarg(env);
}

  
static ERL_NIF_TERM 
bsn_all(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	bsn_res* r;
	unsigned int max, pos = 0;
	ERL_NIF_TERM head, tail;
	ErlNifBinary bin;
	bsn_elem* elem_ptr;
	
	if (!enif_get_resource(env, argv[0], bsn_type, (void**) &r))
		return enif_make_badarg(env);
	tail = enif_make_list(env, 0);

	enif_mutex_lock(r->mutex);
	max = r->max;
	elem_ptr = r->list;

	do {

		if (elem_ptr->hash != max) {
			bin = elem_ptr->bin;
			enif_realloc_binary(&bin, bin.size);
			head = enif_make_binary(env, &bin);
			tail = enif_make_list_cell(env, head, tail);
		}

		elem_ptr++;
		pos++;
	} while (pos < max);

	enif_mutex_unlock(r->mutex);
	
	return tail;
}
  

static ERL_NIF_TERM 
bsn_count(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	bsn_res* r;
	
	if (!enif_get_resource(env, argv[0], bsn_type, (void**) &r))
		return enif_make_badarg(env);

	return enif_make_int(env, r->count);
}


void private_clear_all(bsn_res* r)
{
	unsigned int max, num;
	bsn_elem* ptr;
	num = max = r->max;
	ptr = r->list;

	while (num) {
		if (ptr->hash != max) {
			enif_release_binary(&(ptr->bin));
		}
		ptr++;
		num--;
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
    ATOM_NO_MORE  = enif_make_atom(env, "no_more");

	
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
    {"in",      2, bsn_search},
    {"clear",   2, bsn_clear},
    {"count",   1, bsn_count},
};


ERL_NIF_INIT(bsn_int, nif_functions, &on_load, &on_load, &on_upgrade, NULL);
