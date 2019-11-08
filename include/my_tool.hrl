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

-define(PRINT(Word), io:format("~p~n", [Word])).

% 创建一个调用N次 TestFun() 的函数
-define(RECURSIVE(Num, TestFun),
    begin
        F =
            fun(Fun, Num) when Num > 0 -> TestFun(), Fun(Fun, Num-1);
                (_, 0) -> ok
            end,
        F(F, Num)
    end
).

% 创建一个接收调用次数的函数 @fixme erlc: variable '_Num' shadowed in 'fun'
-define(RECURSIVE_FUN(TestFun), fun(_Num) -> ?RECURSIVE(_Num, TestFun) end).

