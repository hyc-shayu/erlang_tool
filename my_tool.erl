%%%-------------------------------------------------------------------
%%% @author huangyoucai
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 25. 10月 2019 16:59
%%%-------------------------------------------------------------------
-module(my_tool).
-author("huangyoucai").

%% API
-export([
    put_pd_from_proplist/1
]).

put_pd_from_proplist([{Key, Value} | T]) ->
    erlang:put(Key, Value),
    put_pd_from_proplist(T);
put_pd_from_proplist([_| T]) ->
    put_pd_from_proplist(T);
put_pd_from_proplist([]) ->
    ok;
put_pd_from_proplist(Args) ->
    erlang:error(badarg, [Args]).
