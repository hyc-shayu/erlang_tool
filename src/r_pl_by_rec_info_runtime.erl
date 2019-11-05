%%%-------------------------------------------------------------------
%%% @author shayu
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. 11月 2019 18:14
%%%-------------------------------------------------------------------
-module(r_pl_by_rec_info_runtime).
-author("shayu").
-compile([{parse_transform, record_info_runtime}]).

-include("r_pl_by_rec_info_runtime.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([
    r2pl/2,
    pl2r/2
]).

-export([
    record_info_fields/1
]).

-record(foo1, {id=0, name}).

%% API
base_test_() ->
    [
        ?_assert(?PL2R(?R2PL(#foo1{})) =:= #foo1{})
    ].

r2pl(Record, Mod) ->
    case my_is_record(Record, Mod) of
        false -> Record;
        true ->
            RecordName = element(1, Record),
            Fields = [?RECORD_NAME|?RECORD_FIELDS(Mod, RecordName)],
            Values = tuple_to_list(Record),
            r2pl_get_values(Fields, Values, Mod)
    end.

r2pl_get_values(Fields, Values, Mod) ->
    Fun =
        fun(Field, Value) when is_tuple(Value) -> {Field, r2pl(Value, Mod)};
            (Field, Value) when is_list(Value) -> {Field, [r2pl(V, Mod) || V <- Value]};
            (Field, Value) -> {Field, Value}
        end,
    lists:zipwith(Fun, Fields, Values).

pl2r([{?RECORD_NAME, RecordName}|RestPL], Mod) ->
    Fields = ?RECORD_FIELDS(Mod, RecordName),
    list_to_tuple([RecordName|pl2r_get_values(RestPL, Fields, Mod)]);
pl2r(Other, _Mod) -> Other.

pl2r_get_values([_|_]=PL, [Field | RestFields], Mod) ->
    {RestPL, Value} =
        case pl_find(PL, Field) of
            {_RestPL, _Value} -> {_RestPL, pl2r(_Value, Mod)};
            ?NO_VALUE -> {PL, ?NO_VALUE}
        end,
    [Value | pl2r_get_values(RestPL, RestFields, Mod)];
pl2r_get_values([], [_Field|Fields], Mod) ->
    [?NO_VALUE | pl2r_get_values([], Fields, Mod)];
pl2r_get_values(_, [], _Mod) ->
    [].

pl_find(PL, Key) ->
    pl_find(PL, Key, []).
pl_find([{Key, Value}|T], Key, HRest) ->
    {HRest ++ T, Value};
pl_find([H|T], Key, HRest) ->
    pl_find(T, Key, [H|HRest]);
pl_find([], _Key, _Rest) ->
    ?NO_VALUE.

my_is_record(Rec, Mod) when is_tuple(Rec), tuple_size(Rec) >= 1 ->
    RecName = erlang:element(1, Rec),
    is_list(catch Mod:record_info_fields(RecName));

my_is_record(_, _) -> false.
