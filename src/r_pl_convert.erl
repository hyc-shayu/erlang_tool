%%%-------------------------------------------------------------------
%%% @author shayu
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%% 
%%% record proplist 转换方案，采用map存储record字段映射
%%% 参考r_m_convert
%%% record2proplist 比 r2pl 快一丢丢
%%% proplist2record 比 pl2r 快许多
%%% @end
%%% Created : 20. 10月 2019 20:38
%%%-------------------------------------------------------------------
-module(r_pl_convert).
-export([
    r2pl_flatten/2,
    pl2r_recover/2,
    r2pl/2,
    pl2r/2
]).

-export([
    record2proplist/2,
    record2proplist/3,
    proplist2record/2,
    record2proplist_flatten/2,
    record2proplist_flatten/3,
    proplist2record_recover/2
]).

-define(RECORD_NAME, '__record_name').
-define(DEFAULT_DEEP, 10).

%%======================================================================================
%% 检查松散，传入不可转换的参数不会抛出异常（flatten传入非元组除外），返回该参数，对于第一个元素是可转换的list，
%% 不强求所有元素都为同一个record结构，除了头元素，其他元素可以任意类型
%% ---更新---
%% 增加默认值映射选择，使用record名字的字符串映射到默认值列表，可选，如果没有，则新增字段默认值为undefined
%% #{record => record_info(fields, record), "record" => tuple_to_list(#i_friend{})}.
%%======================================================================================
record2proplist_flatten(Record, RecordFieldsMap) ->
    record2proplist_flatten(Record, RecordFieldsMap, ?DEFAULT_DEEP).

record2proplist_flatten(Record, #{} = RecordFieldsMap, Deep) when erlang:is_tuple(Record), erlang:is_integer(Deep) ->
    RL = erlang:tuple_to_list(Record),
    erlang:list_to_tuple([record2proplist(Term, RecordFieldsMap, Deep) || Term <- RL]);
record2proplist_flatten(Record, #{} = RecordFieldsMap, Deep) ->
    erlang:error(badarg, [Record, RecordFieldsMap, Deep]).

record2proplist(Term, RecordFieldsMap) ->
    record2proplist(Term, RecordFieldsMap, ?DEFAULT_DEEP).

record2proplist(Term, RecordFieldsMap, Deep) when Deep > 0 ->
    case my_is_record(Term, RecordFieldsMap) of
        true ->
            record2proplist_0(Term, RecordFieldsMap, Deep - 1);
        false ->
            case my_is_record_list(Term, RecordFieldsMap) of
                false -> Term;
                true ->
                    record_list_to_proplist(Term, RecordFieldsMap, Deep)
            end
    end;
record2proplist(Term, _, 0) -> Term.

record2proplist_0(Record, RecordFieldsMap, Deep) ->
    RecordName = erlang:element(1, Record),
    record2proplist_1(Record, [?RECORD_NAME | maps:get(RecordName, RecordFieldsMap)], 1, RecordFieldsMap, Deep).

record2proplist_1(Record, [Field|TFL], Index, RecordFieldsMap, Deep) ->
    Value = erlang:element(Index, Record),
    Value1 = record2proplist(Value, RecordFieldsMap, Deep),
    [{Field, Value1} | record2proplist_1(Record, TFL, Index+1, RecordFieldsMap, Deep)];
record2proplist_1(_Record, [], _Index, _RecordFieldsMap, _Deep) -> [].

record_list_to_proplist(RL, RecordFieldsMap, Deep) ->
    lists:map(fun(_Term) -> record2proplist(_Term, RecordFieldsMap, Deep) end, RL).



proplist2record_recover(Record, #{} = RecordFieldsMap) when erlang:is_tuple(Record) ->
    RL = erlang:tuple_to_list(Record),
    erlang:list_to_tuple([proplist2record(Term, RecordFieldsMap) || Term <- RL]);
proplist2record_recover(Record, #{}) ->
    erlang:error(badarg, [Record]).

proplist2record([{?RECORD_NAME, RecordName}|TPL], #{} = RecordFieldsMap) ->
    erlang:list_to_tuple([RecordName | get_values_by_fields(TPL, maps:get(RecordName, RecordFieldsMap), get_default_value_list(RecordName, RecordFieldsMap), RecordFieldsMap)]);
proplist2record([[{?RECORD_NAME, _}|_]|_]=PLL, #{} = RecordFieldsMap) ->
    [proplist2record(PL, RecordFieldsMap) || PL <- PLL];
proplist2record(Term, #{}) -> Term.

get_default_value_list(RecordName, RecordFieldsMap) ->
    case maps:get(atom_to_list(RecordName), RecordFieldsMap, undefined) of
        [RecordName|TVL] -> TVL;
        undefined -> undefined
    end.

get_values_by_fields([{Field, Value}|TPL], [Field | TFL], [_ | TVL], RecordFieldsMap) ->
    [proplist2record(Value, RecordFieldsMap) | get_values_by_fields(TPL, TFL, TVL, RecordFieldsMap)];
get_values_by_fields([{_,_}=H|TPL]=PL, [Field|TFL], DefaultValueList, RecordFieldsMap) ->
    case get_values_by_fields_1(TPL, Field, [H]) of
        {RestPL, Value} ->
            [proplist2record(Value, RecordFieldsMap)|get_values_by_fields(RestPL, TFL, DefaultValueList, RecordFieldsMap)];
        not_found ->
            {Default, TVL} =
            case DefaultValueList of
                [_Default|_TVL] ->
                    {_Default, _TVL};
                undefined ->
                    {undefined, undefined}
            end,
            [Default|get_values_by_fields(PL, TFL, TVL, RecordFieldsMap)]
    end;
get_values_by_fields([], [_|TFL], [Default|TVL], RecordFieldsMap) ->
    [Default|get_values_by_fields([], TFL, TVL, RecordFieldsMap)];
get_values_by_fields([], [_|TFL], undefined, RecordFieldsMap) ->
    [undefined|get_values_by_fields([], TFL, undefined, RecordFieldsMap)];
get_values_by_fields(_, [], _, _) -> [].

get_values_by_fields_1([{Field, Value}|TPL], Field, HRest) ->
    {lists:reverse(HRest, TPL), Value};
get_values_by_fields_1([{_,_}=H|TPL], Field, HRest) ->
    get_values_by_fields_1(TPL, Field, [H|HRest]);
get_values_by_fields_1([], _Field, _HRest) ->
    not_found.

%%======================================================================================
%% 进行严格检查，传入的record|proplist无法转换将会抛出异常，如果属性中有list中第一个是可以的转换record|proplist
%% 尾列表所有元素必需与第一个record为同一个record结构，proplist的record_name必须一致，否则抛出错误
%%======================================================================================

r2pl_flatten(Record, #{} = RecordFieldsMap) when erlang:is_tuple(Record) ->
    RL = erlang:tuple_to_list(Record),
    erlang:list_to_tuple([r2pl_term_deal(Term, RecordFieldsMap) || Term <- RL]);
r2pl_flatten(Record, #{}) ->
    erlang:error(badarg, [Record]).
    
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

my_is_record(Record, RecordFieldsMap) when erlang:is_tuple(Record), erlang:is_atom(element(1, Record)) ->
    RecordName = erlang:element(1, Record),
    erlang:is_record(Record, RecordName, erlang:size(Record)) andalso maps:is_key(RecordName, RecordFieldsMap);
my_is_record(_, _) -> false.

my_is_record_list([Record|_], RecordFieldsMap) ->
    my_is_record(Record, RecordFieldsMap);
my_is_record_list(_, _) -> false.



pl2r_recover(Record, #{} = RecordFieldsMap) when erlang:is_tuple(Record) ->
    RL = erlang:tuple_to_list(Record),
    erlang:list_to_tuple([pl2r_get_best_val(Term, RecordFieldsMap) || Term <- RL]);
pl2r_recover(Record, #{}) ->
    erlang:error(badarg, [Record]).

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
