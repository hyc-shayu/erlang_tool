%%%-------------------------------------------------------------------
%%% @author shayu
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%% idea 异步调试 不阻塞主流程
%%% 1.新建一个调试方法 在该方法打上断点
%%% 2.流程中用该方法创建一个进程 对该进程调试
%%% @end
%%% Created : 25. 10月 2019 11:07
%%%-------------------------------------------------------------------
-module(my_debug).
-author("shayu").

%% API
-export([
    example/1
]).

-define(PUT_FROM_PROP_LIST(Args),
    begin
        __Fun =
            fun(__F, [{Key, Value}|T]) -> erlang:put(Key, Value), __F(__F, T);
                (__F, []) -> ok;
                (__F, Args) -> erlang:error(badarg, [Args])
            end,
        __Fun(__Fun, Args)
    end
).

example(Args) ->
    put(a, 1),
    put(b, 2),
    put(c, 3),
    put(d, 4),
    Fun = fun bad_fun/1,
    Condition = debug,
    case Condition of
        debug ->
            % 获取所有进程字典（调试进程复制进程字典使用）
            ProcessDictionary = erlang:get(),
            % 创建一个调试进程 断点可以打在该进程方法(调试用的) 避免拦截主流程(断点打在bad_fun中)、断点混乱
            Process = fun() -> example_debug(ProcessDictionary, Fun, [Args]) end,
            erlang:spawn(Process);
        _ -> continue
    end,
    do_your_work.

example_debug(ProcessDictionary, Fun, ArgsLs) ->
    % 在这个方法打断点 对调试进程进行调试
    ?PUT_FROM_PROP_LIST(ProcessDictionary),
    erlang:apply(Fun, ArgsLs).

bad_fun(Divisor) ->
    A = 1,
    A / Divisor.
