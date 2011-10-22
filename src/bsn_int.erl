-module(bsn_int).

-on_load(init/0).
-export([init/0]).

%% API
-export([new/1, add/2, all/1, in/2, count/1, clear/2]).

-define(NIF_NOT_LOADED, erlang:nif_error(nif_not_loaded)).

init() ->
    erlang:load_nif(code:priv_dir('bsn')++"/bsn_int", 0).

%% Create new resource, `CellCount' is the size of the painters' store.
new(CellsCount) ->
	?NIF_NOT_LOADED.

%% Add new element.
%% If the result is a negative integer 
%% then object was already added.
%% We found this object with (result) steps.
%%
%% If the result is a positive integer 
%% then object was added after (result) elements.
add(Res, Bin) ->
	?NIF_NOT_LOADED.

all(Res) ->
	?NIF_NOT_LOADED.

%% Add new element.
%% If the result is a negative integer 
%% then object was found with (-result) steps.
%%
%% If the result is a positive integer 
%% then object was not found with (result) steps.
in(Res, Bin) ->
	?NIF_NOT_LOADED.

%% Return the count of elements stored in this resource.
count(Res) ->
	?NIF_NOT_LOADED.

clear(Res, Bin) ->
	?NIF_NOT_LOADED.
