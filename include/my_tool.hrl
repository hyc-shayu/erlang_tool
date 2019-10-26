%%%-------------------------------------------------------------------
%%% @author shayu
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 25. 10月 2019 16:48
%%%-------------------------------------------------------------------
-author("shayu").

% 从proplist添加键值对到进程字典
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


% my debug macro
-define(MY_DEBUG(__Fun, __ArgLs),
    begin
    % 获取所有进程字典（调试进程复制进程字典使用）
        __ProcessDictionary = erlang:get(),
        % 创建一个调试进程 断点可以打在该进程方法(调试用的) 避免拦截主流程(断点打在bad_fun中)、断点混乱
        __ProcessFun =
            fun() ->
                ?PUT_FROM_PROP_LIST(__ProcessDictionary),
                erlang:apply(__Fun, __ArgLs)
            end,
        erlang:spawn(__ProcessFun)
    end
).