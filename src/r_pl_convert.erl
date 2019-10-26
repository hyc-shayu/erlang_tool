%%%-------------------------------------------------------------------
%%% @author shayu
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%% 
%%% record proplist 转换方案，采用map存储record字段映射
%%% 参考r_m_convert
%%%
%%% @end
%%% Created : 20. 10月 2019 20:38
%%%-------------------------------------------------------------------
-module(r_pl_convert).
-export([
    r2pl_faltten/2,
    pl2r_recover/2,
    r2pl/2,
    pl2r/2
]).

-export([
    test/0,
    test1/0
]).

-define(Test, lists:zip(lists:seq(1,10000), lists:seq(10001,20000))).

-record(foo, {id=0, name=noname}).

-record(foo1, {id=1, name=ha, age=22, weight=100, username=god, password=666, sd=gfd, hsjdf=erjg, sdhfjs=[dfehs,dfse,weh], score=98, max=9999999, power=9999, foo=#foo{}}).

-record(foo2, {foo1=#foo1{id=5}, foo2=[#foo1{id=6}, #foo1{id=3}, #foo1{id=4}], foo3=#foo1{}, foo4=#foo1{id=7}, foo5=#foo1{id=8}, foo6=#foo1{}, foo7=#foo1{}, foo8=#foo1{}, foo9=#foo1{}, foo10=#foo1{}}).

-record(foo3, {foo1=[#foo1{id=1}, #foo1{id=2}], foo=#foo{}}).

-define(foo1, record_info(fields, foo1)).
-define(RECORD_FIELDS_MAP, #{foo=> record_info(fields, foo), foo1=> record_info(fields, foo1), foo2=> record_info(fields, foo2)}).

-define(RECORD_NAME, '__record_name').

test() ->
    Record = r2pl_faltten(#foo3{}, ?RECORD_FIELDS_MAP),
    io:format("~p~n~n", [Record]),
    Rec = pl2r_recover(Record, ?RECORD_FIELDS_MAP),
    io:format("~p~n~n", [Rec]).

test1() ->
    PL = r2pl_0(#foo1{}, ?RECORD_FIELDS_MAP),
    io:format("~p~n~n", [PL]),
    #foo1{} = Record = pl2r(PL, ?RECORD_FIELDS_MAP),
    io:format("~p~n~n", [Record]).


r_pl_opt(Record, #{}=RecordFieldsMap, Type) when erlang:is_tuple(Record) ->
    RecordName = erlang:element(1, Record),
    Size = erlang:size(Record),
    case {erlang:is_record(Record, RecordName, Size), Type} of
        {false, _} -> erlang:error(badarg, [Record, RecordFieldsMap]);
        {true, 1} -> r2pl_faltten_1(Record, RecordFieldsMap, Size);
        {true, 2} -> pl2r_recover_1(Record, RecordFieldsMap, Size)
    end;
r_pl_opt(Record, RecordFieldsMap, _) -> erlang:error(badarg, [Record, RecordFieldsMap]).

r2pl_faltten(Record, RecordFieldsMap) ->
    r_pl_opt(Record, RecordFieldsMap, 1).
    

r2pl_faltten_1(NewRecord, _RecordFieldsMap, 1) -> NewRecord;
r2pl_faltten_1(Record, RecordFieldsMap, Index) ->
    Term = erlang:element(Index, Record),
    NewTerm = r2pl_term_deal(Term, RecordFieldsMap),
    NewRecord = erlang:setelement(Index, Record, NewTerm),
    r2pl_faltten_1(NewRecord, RecordFieldsMap, Index-1).

r2pl(Record, #{} = RecordFieldsMap) ->
    case my_is_record(Record, RecordFieldsMap) of
        false -> erlang:error(badarg, [Record, RecordFieldsMap]);
        true ->
            r2pl_0(Record, RecordFieldsMap)
    end;
r2pl(_, RecordFieldsMap) -> erlang:error(badarg, [RecordFieldsMap]).

r2pl_0(Record, RecordFieldsMap) ->
    RecordName = erlang:element(1, Record),
    [{?RECORD_NAME, RecordName} | r2pl_1(Record, maps:get(RecordName, RecordFieldsMap), erlang:size(Record), RecordFieldsMap)].

r2pl_1(Record, FL, Index, RecordFieldsMap) ->
    r2pl_2(Record, FL, 2, Index, RecordFieldsMap).

r2pl_2(_Record, _, Index, MaxIndex, _) when Index > MaxIndex -> [];
r2pl_2(Record, [Field | FT], Index, MaxIndex, RecordFieldsMap) ->
    Term = erlang:element(Index, Record),
    NewTerm = r2pl_term_deal(Term, RecordFieldsMap),
    [{Field,NewTerm} | r2pl_2(Record, FT, Index+1, MaxIndex, RecordFieldsMap)].

r2pl_deal_rl([Record|_] = RL, RecordFieldsMap) ->
    RecordName = erlang:element(1, Record),
    Size = erlang:size(Record),
    r2pl_deal_rl_1(RL, RecordFieldsMap, RecordName, Size).

r2pl_deal_rl_1([], _, _, _) -> [];
r2pl_deal_rl_1([Record|T], RecordFieldsMap, RecordName, Size) when erlang:is_tuple(Record), erlang:element(1, Record) =:= RecordName, erlang:size(Record) =:= Size ->
    [r2pl_0(Record, RecordFieldsMap) | r2pl_deal_rl_1(T, RecordFieldsMap, RecordName, Size)];
r2pl_deal_rl_1(RL, _, _RecordName, _Size) -> erlang:error(badarg, [RL]).

r2pl_term_deal(Term, RecordFieldsMap) ->
    case my_is_record(Term, RecordFieldsMap) of
        false ->
            case my_is_record_list(Term, RecordFieldsMap) of
                false -> Term;
                true ->
                    r2pl_deal_rl(Term, RecordFieldsMap)
            end;
        true ->
            r2pl_0(Term, RecordFieldsMap)
    end.
                    
my_is_record(Record, RecordFieldsMap) when erlang:is_tuple(Record) ->
    RecordName = erlang:element(1, Record),
    maps:is_key(RecordName, RecordFieldsMap) andalso erlang:is_record(Record, RecordName, erlang:size(Record));
my_is_record(_, _) -> false.

my_is_record_list([Record|_], RecordFieldsMap) ->
    my_is_record(Record, RecordFieldsMap);
my_is_record_list(_, _) -> false.

pl2r_recover(Record, RecordFieldsMap) ->
    r_pl_opt(Record, RecordFieldsMap, 2).


pl2r_recover_1(NewRecord, _, 1) -> NewRecord;
pl2r_recover_1(Record, RecordFieldsMap, Index) ->
    Term = erlang:element(Index, Record),
    NewTerm = pl2r_get_best_val(Term, RecordFieldsMap),
    NewRecord = erlang:setelement(Index, Record, NewTerm),
    pl2r_recover_1(NewRecord, RecordFieldsMap, Index-1).

pl2r([{?RECORD_NAME, RecordName}|_] = PL, #{} = RecordFieldsMap) ->
    pl2r(PL, [?RECORD_NAME | maps:get(RecordName, RecordFieldsMap)], RecordFieldsMap);
pl2r(PL, RecordFieldsMap) -> erlang:error(badarg, [PL, RecordFieldsMap]).

pl2r([{_,_}|_]=PL, [_|_]=FL, RecordFieldsMap) ->
    erlang:list_to_tuple(pl2r_0(PL, FL, RecordFieldsMap));
pl2r(PL, FL, RecordFieldsMap) -> erlang:error(badarg, [PL, FL, RecordFieldsMap]).

pl2r_0([], [], _RecordFieldsMap) -> [];
pl2r_0([], [_Key, FT], RecordFieldsMap) ->
    [undefined | pl2r_0([], FT, RecordFieldsMap)];
pl2r_0([{_,_}|_], [], _RecordFieldsMap) -> [];
pl2r_0([{Key, Value}|PT], [Key | FT], RecordFieldsMap) ->
    NewValue = pl2r_get_best_val(Value, RecordFieldsMap),
    [NewValue | pl2r_0(PT, FT, RecordFieldsMap)];
pl2r_0([{_,_}|_] = PL, [Key1 | FT], RecordFieldsMap) ->
    {Rest, Value} = pl2r_1(PL, Key1),
    NewValue = pl2r_get_best_val(Value, RecordFieldsMap),
    [NewValue | pl2r_0(Rest, FT, RecordFieldsMap)];
pl2r_0(Arg1, Arg2, RecordFieldsMap) ->
    erlang:error(badarg, [Arg1, Arg2, RecordFieldsMap]).


pl2r_1(PL, Key) ->
    case pl2r_1_1(PL, Key, {[], undefined}) of
        {Rest, Value} -> {Rest, Value};
        undefined -> {PL, undefined}
    end.
        

pl2r_1_1([], _Key, {_Rest, DefaultV}) ->
    DefaultV;
pl2r_1_1([{Key, Value}|PT], Key, {Rest, _V}) ->
    {lists:reverse(Rest) ++ PT, Value};
pl2r_1_1([H|PT], Key, {Rest, V}) ->
    pl2r_1_1(PT, Key, {[H|Rest], V}).

pl2r_pl_list_deal([[{?RECORD_NAME, RecordName}|_]|_] = PLL, RecordFieldsMap) ->
    pl2r_pl_list_deal_1(PLL, RecordFieldsMap, RecordName).

pl2r_pl_list_deal_1([], _, _) -> [];
pl2r_pl_list_deal_1([[{?RECORD_NAME, RecordName}|_] = PLH | PLT], RecordFieldsMap, RecordName) ->
    [pl2r(PLH, RecordFieldsMap) | pl2r_pl_list_deal_1(PLT, RecordFieldsMap, RecordName)];
pl2r_pl_list_deal_1(PLL, _, _) -> erlang:error(badarg, [PLL]).

pl2r_get_best_val(Value, RecordFieldsMap) ->
    case my_is_pl(Value, RecordFieldsMap) of
        false -> 
            case my_is_pl_list(Value, RecordFieldsMap) of
                false -> Value;
                true ->
                    pl2r_pl_list_deal(Value, RecordFieldsMap)
            end;
        true -> 
            pl2r(Value, RecordFieldsMap)
    end.

my_is_pl([{?RECORD_NAME, RecordName}|_], #{}=RecordFieldsMap) -> 
    maps:is_key(RecordName, RecordFieldsMap);
my_is_pl(_, _) -> false.

my_is_pl_list([PL | _], RecordFieldsMap) ->
    my_is_pl(PL, RecordFieldsMap);
my_is_pl_list(_, _) -> false.
