-module(bsn).

%% API
-export([hash/2, compare/2]).
-export([new/2, add/2, all/1, chains/1, in/2, count/1, clear/2]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
%-include_lib("triq/include/triq.hrl").
-endif.


%% Create new resource, `CellCount' is the size of the painters' store.
new('int_quadric', CellsCount) when CellsCount > 0 ->
	{'bsn_int', bsn_int:new(-CellsCount)};

new('int_linear', CellsCount) when CellsCount > 0 ->
	{'bsn_int', bsn_int:new(CellsCount)};

new('ext', CellsCount) when CellsCount > 0 ->
	{'bsn_ext', bsn_ext:new(CellsCount)}.




%% Add new element.
%% If the result is a negative integer 
%% then object was already added.
%% We found this object with (result) steps.
%%
%% If the result is a positive integer 
%% then object was added after (result) elements.
add({Type, Res}, Bin) ->
	Type:add(Res, Bin).

all({Type, Res}) ->
	Type:all(Res).

chains({Type, Res}) ->
	Type:chains(Res).

%% Add new element.
%% If the result is a negative integer 
%% then object was found with (-result) steps.
%%
%% If the result is a positive integer 
%% then object was not found with (result) steps.
in({Type, Res}, Bin) ->
	Type:in(Res, Bin).
    
clear({Type, Res}, Bin) ->
	Type:clear(Res, Bin).

%% Return the count of elements stored in this resource.
count({Type, Res}) ->
	Type:count(Res).

%% Calculate the hash of the  binary
hash(Bin, Max) ->
	bsn_ext:hash(Bin, Max).
	
compare(Bin1, Bin2) ->
	bsn_ext:compare(Bin1, Bin2).

-ifdef(TEST).
-ifdef(FORALL).
prop_compare_test_() ->
    {"Binary compare testing.",
    	{timeout, 60,
    		fun() -> triq:check(prop_compare()) end}}.

prop_compare() ->
   ?FORALL({Xs},{binary()},
   	    compare(Xs, Xs)).
-endif.

-endif.
