%%%-------------------------------------------------------------------
%%% @author shayu
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%% 
%%% 新增接收record字段映射map的接口 嵌套的所有需要转换的record都必须在该map中
%%%
%%% record map 相互非嵌套转换
%%% 嵌套record map 相互转换解决方案 使用map存储record名字和字段的映射
%%%
%%% @end
%%% Created : 17. 10月 2019 22:38
%%%-------------------------------------------------------------------
-module(r_m_convert).

-export([
    record_to_map/2,
    record_flatten_to_map/2,
    map_to_record/2,
    record_recover_from_map/2
]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 
%%  record map 相互非嵌套转换 嵌套时只会转换最外一层
%%  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-define(RECORD_2_MAP(RecordName, Record),
    begin
        [RecordName | _Rest] = tuple_to_list(Record),
        maps:from_list(lists:zip(record_info(fields, RecordName), _Rest))
    end
).
-define(MAP_2_RECORD(Map, RecordName), 
	list_to_tuple([RecordName | [maps:get(_Key, Map, undefined) || _Key <- record_info(fields, RecordName)]]) 
).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 
%%  record是在编译前进行预处理转换成tuple，运行时并没有record的概念
%%  因此record_info()无法使用变量参数在运行时获取字段
%%  
%%  可以采用map的方式来维护record名字到record字段的映射，在处理嵌套结构时动态获取
%%  所有需要转换的record都需要在该map中添加好映射
%%
%%  如果中间的record没在映射表中，那么它以及里面的record都不会被转换（即使内部record在record_fields映射中）
%%  
%%  如果record元素类型是集合（list例外），里面的record不会被转换，map同理，例如
%%  #foo{
%%      list = [#foo1{}, #foo2{}],
%%      tuple = {#foo1{}},
%%      map = #{#foo1{} =>#foo2{}}
%%  }.
%%  考虑到list在record中的使用场景，list中的record全都相同时也可以被转换
%%  但严格要求list中必须都是同一种record（如果第一个record可转换），上面的list中存在不同record的情况将会抛出badarg
%%  #foo{
%%      list = [#foo1{}, #foo1{}, #foo1{}]
%%  }.
%%
%%  tuple和map中放入了record，并不会被转换，也不会异常
%%
%%  如果tuple中的是同样的record，那么可以使用list来达到转换的目的
%%  如果tuple中存在不同的record，建议将它们提取成record的单独字段，相同的record用list来保存成一个字段
%%  
%%  如果map中放入了record，建议将键提取成字段
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% 统一map存储record名字的key
-define(RECORD_NAME, '__record_name').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 
%%  接收record 内部嵌套record转换成嵌套map
%%  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
record_flatten_to_map(Record, #{} =RecordFieldsMap) when erlang:is_tuple(Record)->
    RecordName = erlang:element(1, Record),
    case erlang:is_record(Record, RecordName, erlang:size(Record)) of
        false -> erlang:error(badarg, [Record, RecordFieldsMap]);
        true ->
            record_flatten_to_map_1(Record, RecordFieldsMap)
    end;
record_flatten_to_map(Record, #{}) -> erlang:error(badarg, [Record]).

record_flatten_to_map_1(Record, RecordFieldsMap) ->
    RL = erlang:tuple_to_list(Record),
    erlang:list_to_tuple([r2m_get_best_term(Term, RecordFieldsMap) || Term <- RL]).
%%record_flatten_to_map_1(Record, _, 1) -> Record;
%%record_flatten_to_map_1(Record, RecordFieldsMap, Index) ->
%%    Term = erlang:element(Index, Record),
%%    NewTerm = r2m_get_best_term(Term, RecordFieldsMap),
%%    NewRecord = erlang:setelement(Index, Record, NewTerm),
%%    record_flatten_to_map_1(NewRecord, RecordFieldsMap, Index-1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 
%%  嵌套record转换成嵌套map
%%  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
record_to_map(Record, RecordFieldsMap) ->
    case my_is_record(Record, RecordFieldsMap) of
        true ->
            record_to_map_1(Record, RecordFieldsMap);
        false ->
            erlang:error(badarg, [Record])
    end.

record_to_map_1(Record, RecordFieldsMap) ->
    RecordName = erlang:element(1, Record),
	r2m_map([?RECORD_NAME | maps:get(RecordName, RecordFieldsMap)], r2m_get_val_list(Record, RecordFieldsMap, erlang:size(Record), []), #{}).

r2m_map([Field|TFL], [Value|TVL], Map) ->
    r2m_map(TFL, TVL, Map#{Field => Value});
r2m_map([], [], Map) -> Map.

r2m_get_val_list(_Record, _RecordFieldsMap, 0, Ret) -> Ret;
r2m_get_val_list(Record, RecordFieldsMap, Index, Ret) ->
    Term = erlang:element(Index, Record),
    Val = r2m_get_best_term(Term, RecordFieldsMap),
    r2m_get_val_list(Record, RecordFieldsMap, Index-1, [Val | Ret]).

record_list_to_map_list([Record|_] = L, RecordFieldsMap) ->
    RecordName = erlang:element(1, Record),
    Size = erlang:size(Record),
    case catch [record_list_to_map_list_1(Record1, RecordName, Size, RecordFieldsMap) || Record1 <- L] of
        {error, bad_record_list_element} -> erlang:error(badarg, [L]);
        Result -> Result
    end.

record_list_to_map_list_1(Record, RecordName, RecordSize, RecordFieldsMap) when erlang:is_tuple(Record) ->
    case {erlang:element(1, Record), erlang:size(Record)} of
        {RecordName, RecordSize} ->
            record_to_map(Record, RecordFieldsMap);
        _ -> erlang:throw({error, bad_record_list_element})
    end;
record_list_to_map_list_1(_, _, _, _) -> erlang:throw({error, bad_record_list_element}).

my_is_record_list([Record|_], RecordFieldsMap) ->
    my_is_record(Record, RecordFieldsMap);
my_is_record_list(_, _) -> false.

my_is_record(Record, RecordFieldsMap) when erlang:is_tuple(Record) ->
    RecordName = erlang:element(1, Record),
    maps:is_key(RecordName, RecordFieldsMap) andalso erlang:is_record(Record, RecordName, erlang:size(Record));
my_is_record(_, _) -> false.

r2m_get_best_term(Term, RecordFieldsMap) ->
    case my_is_record(Term, RecordFieldsMap) of
        true ->
			record_to_map_1(Term, RecordFieldsMap);
        false ->
            case my_is_record_list(Term, RecordFieldsMap) of 
                false -> Term;
                true -> record_list_to_map_list(Term, RecordFieldsMap)
            end
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 
%%  接收record 内部嵌套map转换成嵌套record
%%  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
record_recover_from_map(Record, #{}=RecordFieldsMap) when erlang:is_tuple(Record) ->
    RecordName = erlang:element(1, Record),
    case erlang:is_record(Record, RecordName, erlang:size(Record)) of
        false -> erlang:error(badarg, [record, RecordFieldsMap]);
        true ->
            record_recover_from_map_1(Record, RecordFieldsMap)
    end;
record_recover_from_map(Record, #{}) ->erlang:error(badarg, [Record]).

record_recover_from_map_1(Record, RecordFieldsMap) ->
    RL = erlang:tuple_to_list(Record),
    erlang:list_to_tuple([m2r_get_best_term(Term, RecordFieldsMap) || Term <- RL]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 
%%  嵌套map转换成嵌套record
%%  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
map_to_record(#{?RECORD_NAME := _} = Map, RecordFieldsMap) ->
    map_to_record_1(Map, RecordFieldsMap);
map_to_record(Map, RecordFieldsMap) -> erlang:error(badarg, [Map, RecordFieldsMap]).

map_to_record_1(#{?RECORD_NAME := RecordName} = Map, RecordFieldsMap) ->
    list_to_tuple(m2r_get_val_list(Map, RecordFieldsMap, [?RECORD_NAME | maps:get(RecordName, RecordFieldsMap)], [])).

m2r_get_val_list(_Map, _RecordFieldsMap, [], Ret) -> lists:reverse(Ret);
m2r_get_val_list(Map, RecordFieldsMap, [Field|FT], Ret) ->
    Term = maps:get(Field, Map, undefined),
    Val = m2r_get_best_term(Term, RecordFieldsMap),
    m2r_get_val_list(Map, RecordFieldsMap, FT, [Val|Ret]).

map_list_to_record_list([#{?RECORD_NAME := RecordName} |_] = ML, RecordFieldsMap) ->
    case catch [map_list_to_record_list_1(Map, RecordName, RecordFieldsMap) || Map <- ML] of
        {error, bad_map_list_element} -> erlang:error(badarg, [ML]);
        ResultList -> ResultList
    end.

map_list_to_record_list_1(Map = #{?RECORD_NAME := RecordName}, RecordName, RecordFieldsMap) ->
    map_to_record(Map, RecordFieldsMap);
map_list_to_record_list_1(_, _, _) -> erlang:throw({error, bad_map_list_element}).

my_is_map_list([Map|_]) ->
    my_is_map(Map);
my_is_map_list(_) -> false.

my_is_map(#{?RECORD_NAME := _RecordName}) ->
    true;
my_is_map(_) -> false.

m2r_get_best_term(Term, RecordFieldsMap) ->
    case my_is_map(Term) of
        true ->
            map_to_record_1(Term, RecordFieldsMap);
        false ->
            case my_is_map_list(Term) of
                false -> Term;
                true -> map_list_to_record_list(Term, RecordFieldsMap)
            end
    end.

