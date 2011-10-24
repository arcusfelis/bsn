-module(bsn_measure).
-export([gen/2, check_type/3]).

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
	% Build resourse
	F = fun() -> bsn:new(Type, Size) end,

	[do_check(F, Size, InOutK, 0.1),
	 do_check(F, Size, InOutK, 0.25),
 	 do_check(F, Size, InOutK, 0.5),
	 do_check(F, Size, InOutK, 0.75),
	 do_check(F, Size, InOutK, 0.9),
	 do_check(F, Size, InOutK, 1)].

do_check(F, Size, InOutK, CapacityK) ->
	Res = F(),
	ElemCount = Size * CapacityK,
	{CaseList, ElemList} = gen(ElemCount, InOutK),
	fill_values(Res, ElemList),
	VaList = check_values(Res, CaseList, []),
	{MissList, InNegList} = lists:partition(fun(X) -> X>0 end, VaList),
	InList = lists:map(fun erlang:'-'/1, InNegList),
	AllList = InList ++ MissList,
	{CapacityK, 
		{miss, round4(average(MissList))}, 
		{in, round4(average(InList))}, 
		{all, round4(average(AllList))}}.

	
average([]) ->
    false;
average([X|Tail]) ->
    average1(Tail, X, 1).
% @private
average1([X|Tail], Sum, Count) ->
    average1(Tail, Sum + X, Count + 1);
average1([], Sum, Count) ->
    Sum / Count.


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
	bsn:add(Res, integer_to_binary(H)),
	fill_values(Res, T);
fill_values(Res, []) ->
	Res.

integer_to_binary(X) ->
	erlang:list_to_binary(erlang:integer_to_list(X)).
