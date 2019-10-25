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
                (__F, []) -> ok;
                (__F, Args) -> erlang:error(badarg, [Args])
            end,
        __Fun(__Fun, Args)
    end
).
