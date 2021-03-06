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
-module(idea_debug_mode).
-author("shayu").

%% API
-export([
    example/1
]).

% 从proplist 设置进程字典（复制进程字典）
-define(PUT_FROM_PROP_LIST(Args),
    begin
        __Fun =
            fun(__F, [{Key, Value}|T]) -> erlang:put(Key, Value), __F(__F, T);
                (__F, [_|T]) -> __F(__F, T);
                (__F, []) -> ok;
                (__F, Args) -> erlang:error(badarg, [Args])
            end,
        __Fun(__Fun, Args)
    end
).

-define(MY_DEBUG(_Fun, _ArgLs),
    begin
        % 获取所有进程字典（调试进程复制进程字典使用）
        _ProcessDictionary = erlang:get(),
        % 创建一个调试进程 断点可以打在该进程方法(调试用的) 避免拦截主流程(断点打在bad_fun中)、断点混乱
        _ProcessFun =
            fun() ->
                ?PUT_FROM_PROP_LIST(_ProcessDictionary),
                erlang:apply(_Fun, _ArgLs)
            end,
        erlang:spawn(_ProcessFun)
    end
).

example(Args) ->
    put(a, 1),
    put(b, 2),
    put(c, 3),
    put(d, 4),
    Condition = debug,
    case Condition of
        debug ->
            BadFun = fun bad_fun/1,
            %           debug进程方法        debug进程方法参数
            %                              目标方法   参数
            ?MY_DEBUG(fun example_debug/2, [BadFun, [Args]]);
        _ -> continue
    end,
    do_your_work.

example_debug(Fun, ArgsLs) ->
    % 在这个方法打断点 对调试进程进行调试
    erlang:apply(Fun, ArgsLs).

bad_fun(Divisor) ->
    A = 1,
    A / Divisor.
