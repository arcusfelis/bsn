-module(bsn_measure).
-export([test/0, test2/0, test3/0, print/0]).
-export([gen/2, check_type/4]).
-export([check_type/3, get_type/3, test_type/2]).
-export([check_degrade/0, test_filled/1]).

-ifndef(TEST).
-define(TEST, e).
-endif.
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
%-include_lib("triq/include/triq.hrl").
-endif.


% InOutK is (success / failure) checks.
% Return {TestCases, Elements}.
gen(ElemCount, InOutK) 
	when ElemCount>0 ->
	Nums = lists:seq(0, erlang:round(ElemCount*100)),
	filter(ElemCount, InOutK, Nums, [], []).


filter(EC, InOutK, [H|T], AllAcc, ElemAcc) 
	when EC>0 ->
	case random:uniform() of
	X when X<InOutK ->
		filter(EC-1, InOutK, 
			T, [H|AllAcc], [H|ElemAcc]);
	_X ->
		filter(EC, InOutK, 
			T, [H|AllAcc], ElemAcc)
	end;
filter(_ElemCount, _InOutK, _Acc, AllAcc, ElemAcc) ->
	{AllAcc, ElemAcc}.


check_type(Type, Size, InOutK) ->
	check_type(fun average/1, Type, Size, InOutK).

get_type(Type, Size, InOutK) ->
	check_type(fun(X) -> X end, Type, Size, InOutK).

check_type(OutF, Type, Size, InOutK) ->
	% Build resourse
	F = fun() -> bsn:new(Type, Size) end,

	[do_check(OutF, F, Size, InOutK, 0.1),
	 do_check(OutF, F, Size, InOutK, 0.25),
 	 do_check(OutF, F, Size, InOutK, 0.5),
	 do_check(OutF, F, Size, InOutK, 0.75),
	 do_check(OutF, F, Size, InOutK, 0.9),
	 do_check(OutF, F, Size, InOutK, 1)].

do_check(OutF, F, Size, InOutK, CapacityK) ->
	Res = F(),
	ElemCount = Size * CapacityK,
	{CaseList, ElemList} = gen(ElemCount, InOutK),
	fill_values(Res, ElemList),
	VaList = check_values(Res, CaseList, []),
	{MissList, InNegList} = lists:partition(fun(X) -> X>0 end, VaList),
	InList = lists:map(fun erlang:'-'/1, InNegList),
	AllList = InList ++ MissList,
	{CapacityK, 
		{size, Size}, 
		{real_count, bsn:count(Res)}, 
		{miss, OutF(MissList)}, 
		{in,   OutF(InList)}, 
		{all,  OutF(AllList)}}.

	
average([]) ->
    false;
average([X|Tail]) ->
    average1(Tail, X, 1).
% @private
average1([X|Tail], Sum, Count) ->
    average1(Tail, Sum + X, Count + 1);
average1([], Sum, Count) ->
    round4(Sum / Count).




round4(X) when is_number(X) ->
	erlang:round(X * 1000) / 1000;
round4(X) ->
	X. 
	
check_values(Res, [H|T], Acc) ->
	X = bsn:in(Res, integer_to_binary(H)),
	check_values(Res, T, [X|Acc]);
check_values(_Res, [], Acc) ->
	Acc.
	
fill_values(Res, [H|T]) ->
	case bsn:add(Res, integer_to_binary(H)) of
	no_more ->
		Res;
	X ->
		fill_values(Res, T)
	end;
fill_values(Res, []) ->
	Res.
	
fill_values(Res, [H|T], Acc) ->
	case bsn:add(Res, integer_to_binary(H)) of
	no_more ->
		Acc;
	X ->
		fill_values(Res, T, [H|Acc])
	end;
fill_values(_Res, [], Acc) ->
	Acc.

integer_to_binary(X) ->
	erlang:list_to_binary(erlang:integer_to_list(X)).

test() ->
	[{ext, check_type(ext, 100, 0.5)}
	,{int_linear, check_type(int_linear, 100, 0.5)}
	,{int_quadric, check_type(int_quadric, 100, 0.5)}].

%% All values.
test2() ->
	[{ext, get_type(ext, 100, 0.5)}
	,{int_linear, get_type(int_linear, 100, 0.5)}
	,{int_quadric, get_type(int_quadric, 100, 0.5)}].

%% Counts of values.
test3() ->
	F = fun anal_values/1,
	[{ext, check_type(F, ext, 100, 0.5)}
	,{int_linear, check_type(F, int_linear, 100, 0.5)}
	,{int_quadric, check_type(F, int_quadric, 100, 0.5)}].

print() ->
	do_print(test3()).

do_print([{Type, Vals}|T]) ->
	io:format("Type ~w~n", [Type]),
	lists:map(fun({K,
		{real_count,RC},
		{miss, M},
		{in, I},
		{all, A}}) ->
		io:format("K=~w, RC=~w~n", [K, RC]),
		io:format("count,miss,in,all\n"),

		print_mia(lists:seq(1, 100), M, I, A), 
		io:format("\n") 
	end, Vals),
	do_print(T);
do_print([]) ->
	ok.

print_mia([H|T], [{H,0}|T1], [{H,0}|T2], [{H,0}|T3]) ->
	print_mia(T, T1, T2, T3);
print_mia([H|T], [{H,C1}|T1], [{H,C2}|T2], [{H,C3}|T3]) ->
	io:format("~w,~w,~w,~w\n", [H, C1, C2, C3]),
	print_mia(T, T1, T2, T3);

print_mia([H|_]=L, [{X,_}|_]=L1, L2, L3) 
	when X =/= H ->
	print_mia(L, [{H,0}|L1], L2, L3);
print_mia([H|_]=L, [], L2, L3) ->
	print_mia(L, [{H,0}], L2, L3);

print_mia([H|_]=L, L1, [{X,_}|_]=L2, L3)
	when X =/= H ->
	print_mia(L, L1, [{H,0}|L2], L3);
print_mia([H|_]=L, L1, [], L3) ->
	print_mia(L, L1, [{H,0}], L3);

print_mia([H|_]=L, L1, L2, L3) ->
	print_mia(L, L1, L2, [{H,0}|L3]);
print_mia([], _, _, _) ->
	ok.
	
	

	
	


anal_values(L) ->
	do_anal(lists:sort(L), 1, []).

do_anal([H,H|T], C, Acc) ->
	do_anal([H|T], C+1, Acc);
do_anal([OldH|T], C, Acc) ->
	do_anal(T, 1, [{OldH, C}|Acc]);
do_anal([], C, Acc) ->
	lists:reverse(Acc).

avg(L) -> do_avg(L, 0, 0).
do_avg([H|T], Cnt, Sum) ->
	do_avg(T, Cnt+1, Sum+H);
do_avg([], Cnt, Sum) ->
	Sum / Cnt.
	
check_degrade() ->
	[do_check_degrade(ext)
	,do_check_degrade(int_linear)
	,do_check_degrade(int_quadric)
	].

do_check_degrade(Type) ->
	OutF = fun avg/1,
	[Type,
		lists:map(fun(Size) ->
			F = fun() -> bsn:new(Type, Size) end,
			do_check(OutF, F, Size, 0.5, 1)
			end, [10, 100, 500, 1000, 5000, 10000])].

test_filled(ElemCount) ->
	Res = bsn:new(ext, ElemCount),
	{CaseList, ElemList} = gen(ElemCount, 1),
	Vals = fill_values(Res, ElemList, []),
	{bsn_ext, R} = Res,
	R.

-ifdef(TEST).
	
do_test_() ->
	[?_assert(test_type(bsn:new(ext, 100), 100))
	,?_assert(test_type(bsn:new(int_linear, 100), 100))
	,?_assert(test_type(bsn:new(int_quadric, 100), 100))
	].
-endif.

test_type(Res, ElemCount) ->
	{CaseList, ElemList} = gen(ElemCount, 1),
	Vals = fill_values(Res, ElemList, []),
	%Vals = ElemList,
	lists:all(fun(X) -> bsn:in(Res, integer_to_binary(X)) < 0 end, Vals).
