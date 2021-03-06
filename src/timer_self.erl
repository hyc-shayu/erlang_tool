%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1996-2016. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% %CopyrightEnd%
%%
-module(timer_self).

-export([apply_after/5,
    send_after/3, send_after/4,
    exit_after/3, exit_after/4,
    kill_after/2, kill_after/3,
    apply_interval/5,
    send_interval/3, send_interval/4,
    cancel/2,
    sleep/1,
    tc/1, tc/2, tc/3,
    now_diff/2,
    seconds/1, minutes/1, hours/1, hms/3]).

-export([start_link/1, start/1,
    handle_call/3,  handle_info/2,
    init/1,
    code_change/3, handle_cast/2, terminate/2]).

%% internal exports for test purposes only
-export([get_status/0]).

%% types which can be used by other modules
-export_type([tref/0]).

%% Max
-define(STR_MODULE, atom_to_list(?MODULE)).
-define(MAX_TIMEOUT, 16#0800000).
-define(TIMER_TAB, my_timer_tab).
-define(INTERVAL_TAB, my_timer_interval_tab).

%%
%% Time is in milliseconds.
%%
-opaque tref()    :: {integer(), reference()}.
-type time()      :: non_neg_integer().

%%
%% Interface functions
%%
-spec apply_after(TimerName, Time, Module, Function, Arguments) ->
    {'ok', TRef} | {'error', Reason} when
    TimerName :: atom(),
    Time :: time(),
    Module :: module(),
    Function :: atom(),
    Arguments :: [term()],
    TRef :: tref(),
    Reason :: term().

apply_after(TimerName, Time, M, F, A) ->
    req(TimerName, apply_after, {Time, {M, F, A}}).

-spec send_after(TimerName, Time, Pid, Message) -> {'ok', TRef} | {'error', Reason} when
    TimerName :: atom(),
    Time :: time(),
    Pid :: pid() | (RegName :: atom()),
    Message :: term(),
    TRef :: tref(),
    Reason :: term().
send_after(TimerName, Time, Pid, Message) ->
    req(TimerName, apply_after, {Time, {?MODULE, send, [Pid, Message]}}).

-spec send_after(TimerName, Time, Message) -> {'ok', TRef} | {'error', Reason} when
    TimerName :: atom(),
    Time :: time(),
    Message :: term(),
    TRef :: tref(),
    Reason :: term().
send_after(TimerName, Time, Message) ->
    send_after(TimerName, Time, self(), Message).

-spec exit_after(TimerName, Time, Pid, Reason1) -> {'ok', TRef} | {'error', Reason2} when
    TimerName :: atom(),
    Time :: time(),
    Pid :: pid() | (RegName :: atom()),
    TRef :: tref(),
    Reason1 :: term(),
    Reason2 :: term().
exit_after(TimerName, Time, Pid, Reason) ->
    req(TimerName, apply_after, {Time, {erlang, exit, [Pid, Reason]}}).

-spec exit_after(TimerName, Time, Reason1) -> {'ok', TRef} | {'error', Reason2} when
    TimerName :: atom(),
    Time :: time(),
    TRef :: tref(),
    Reason1 :: term(),
    Reason2 :: term().
exit_after(TimerName, Time, Reason) ->
    exit_after(TimerName, Time, self(), Reason).

-spec kill_after(TimerName, Time, Pid) -> {'ok', TRef} | {'error', Reason2} when
    TimerName :: atom(),
    Time :: time(),
    Pid :: pid() | (RegName :: atom()),
    TRef :: tref(),
    Reason2 :: term().
kill_after(TimerName, Time, Pid) ->
    exit_after(TimerName, Time, Pid, kill).

-spec kill_after(TimerName, Time) -> {'ok', TRef} | {'error', Reason2} when
    TimerName :: atom(),
    Time :: time(),
    TRef :: tref(),
    Reason2 :: term().
kill_after(TimerName, Time) ->
    exit_after(TimerName, Time, self(), kill).

-spec apply_interval(TimerName, Time, Module, Function, Arguments) ->
    {'ok', TRef} | {'error', Reason} when
    TimerName :: atom(),
    Time :: time(),
    Module :: module(),
    Function :: atom(),
    Arguments :: [term()],
    TRef :: tref(),
    Reason :: term().
apply_interval(TimerName, Time, M, F, A) ->
    req(TimerName, apply_interval, {Time, self(), {M, F, A}}).

-spec send_interval(TimerName, Time, Pid, Message) ->
    {'ok', TRef} | {'error', Reason} when
    TimerName :: atom(),
    Time :: time(),
    Pid :: pid() | (RegName :: atom()),
    Message :: term(),
    TRef :: tref(),
    Reason :: term().
send_interval(TimerName, Time, Pid, Message) ->
    req(TimerName, apply_interval, {Time, Pid, {?MODULE, send, [Pid, Message]}}).

-spec send_interval(TimerName, Time, Message) -> {'ok', TRef} | {'error', Reason} when
    TimerName :: atom(),
    Time :: time(),
    Message :: term(),
    TRef :: tref(),
    Reason :: term().
send_interval(TimerName, Time, Message) ->
    send_interval(TimerName, Time, self(), Message).

-spec cancel(TimerName, TRef) -> {'ok', 'cancel'} | {'error', Reason} when
    TimerName :: atom(),
    TRef :: tref(),
    Reason :: term().
cancel(TimerName, BRef) ->
    req(TimerName, cancel, BRef).

-spec sleep(Time) -> 'ok' when
    Time :: timeout().
sleep(T) ->
    receive
    after T -> ok
    end.

%%
%% Measure the execution time (in microseconds) for Fun().
%%
-spec tc(Fun) -> {Time, Value} when
    Fun :: function(),
    Time :: integer(),
    Value :: term().
tc(F) ->
    T1 = erlang:monotonic_time(),
    Val = F(),
    T2 = erlang:monotonic_time(),
    Time = erlang:convert_time_unit(T2 - T1, native, microsecond),
    {Time, Val}.

%%
%% Measure the execution time (in microseconds) for Fun(Args).
%%
-spec tc(Fun, Arguments) -> {Time, Value} when
    Fun :: function(),
    Arguments :: [term()],
    Time :: integer(),
    Value :: term().
tc(F, A) ->
    T1 = erlang:monotonic_time(),
    Val = apply(F, A),
    T2 = erlang:monotonic_time(),
    Time = erlang:convert_time_unit(T2 - T1, native, microsecond),
    {Time, Val}.

%%
%% Measure the execution time (in microseconds) for an MFA.
%%
-spec tc(Module, Function, Arguments) -> {Time, Value} when
    Module :: module(),
    Function :: atom(),
    Arguments :: [term()],
    Time :: integer(),
    Value :: term().
tc(M, F, A) ->
    T1 = erlang:monotonic_time(),
    Val = apply(M, F, A),
    T2 = erlang:monotonic_time(),
    Time = erlang:convert_time_unit(T2 - T1, native, microsecond),
    {Time, Val}.

%%
%% Calculate the time difference (in microseconds) of two
%% erlang:now() timestamps, T2-T1.
%%
-spec now_diff(T2, T1) -> Tdiff when
    T1 :: erlang:timestamp(),
    T2 :: erlang:timestamp(),
    Tdiff :: integer().
now_diff({A2, B2, C2}, {A1, B1, C1}) ->
    ((A2-A1)*1000000 + B2-B1)*1000000 + C2-C1.

%%
%% Convert seconds, minutes etc. to milliseconds.
%%
-spec seconds(Seconds) -> MilliSeconds when
    Seconds :: non_neg_integer(),
    MilliSeconds :: non_neg_integer().
seconds(Seconds) ->
    1000*Seconds.
-spec minutes(Minutes) -> MilliSeconds when
    Minutes :: non_neg_integer(),
    MilliSeconds :: non_neg_integer().
minutes(Minutes) ->
    1000*60*Minutes.
-spec hours(Hours) -> MilliSeconds when
    Hours :: non_neg_integer(),
    MilliSeconds :: non_neg_integer().
hours(Hours) ->
    1000*60*60*Hours.
-spec hms(Hours, Minutes, Seconds) -> MilliSeconds when
    Hours :: non_neg_integer(),
    Minutes :: non_neg_integer(),
    Seconds :: non_neg_integer(),
    MilliSeconds :: non_neg_integer().
hms(H, M, S) ->
    hours(H) + minutes(M) + seconds(S).

%%
%%   Start/init functions
%%

%%   Start is only included because of backward compatibility!
-spec start(atom()) -> 'ok'.
start(TimerName) ->
    ensure_started(TimerName).

-spec start_link(TimerName :: atom()) -> {'ok', pid()} | {'error', term()}.
start_link(TimerName) ->
    gen_server:start_link({local, TimerName}, ?MODULE, [], []).

-spec init([]) -> {'ok', [], 'infinity'}.
init([]) ->
    process_flag(trap_exit, true),
    TimerTab = ets:new(?TIMER_TAB, [ordered_set,protected]),
    IntervalTab = ets:new(?INTERVAL_TAB, [protected]),
    put(?TIMER_TAB, TimerTab),
    put(?INTERVAL_TAB, IntervalTab),
    {ok, [], infinity}.

-spec ensure_started(atom()) -> 'ok'.
ensure_started(TimerName) ->
    case whereis(TimerName) of
        undefined ->
            C = {TimerName, {?MODULE, start_link, []}, permanent, 1000,
                worker, [?MODULE]},
            _ = supervisor:start_child(kernel_safe_sup, C),
            ok;
        _ -> ok
    end.

%% server calls

req(TimerName, Req, Arg) ->
    SysTime = system_time(),
    ensure_started(TimerName),
    gen_server:call(TimerName, {Req, Arg, SysTime}, infinity).
%%    gen_server:call(?TIMER_SERVER, {Req, Arg, SysTime}, infinity).

%%
%% handle_call(Request, From, Timers) ->
%%  {reply, Response, Timers, Timeout}
%%
%% Time and Timeout is in milliseconds. Started is in microseconds.
%%
-type timers() :: term(). % XXX: refine?

-spec handle_call(term(), term(), timers()) ->
    {'reply', term(), timers(), timeout()} | {'noreply', timers(), timeout()}.
handle_call({apply_after, {Time, Op}, Started}, _From, _Ts)
    when is_integer(Time), Time >= 0 ->
    BRef = {Started + 1000*Time, make_ref()},
    Timer = {BRef, timeout, Op},
    ets:insert(get(?TIMER_TAB), Timer),
    Timeout = timer_timeout(system_time()),
    {reply, {ok, BRef}, [], Timeout};
handle_call({apply_interval, {Time, To, MFA}, Started}, _From, _Ts)
    when is_integer(Time), Time >= 0 ->
    %% To must be a pid or a registered name
    case get_pid(To) of
        Pid when is_pid(Pid) ->
            catch link(Pid),
            SysTime = system_time(),
            Ref = make_ref(),
            BRef1 = {interval, Ref},
            Interval = Time*1000,
            BRef2 = {Started + Interval, Ref},
            Timer = {BRef2, {repeat, Interval, Pid}, MFA},
            ets:insert(get(?INTERVAL_TAB), {BRef1,BRef2,Pid}),
            ets:insert(get(?TIMER_TAB), Timer),
            Timeout = timer_timeout(SysTime),
            {reply, {ok, BRef1}, [], Timeout};
        _ ->
            {reply, {error, badarg}, [], next_timeout()}
    end;
handle_call({cancel, BRef = {_Time, Ref}, _}, _From, Ts)
    when is_reference(Ref) ->
    delete_ref(BRef),
    {reply, {ok, cancel}, Ts, next_timeout()};
handle_call({cancel, _BRef, _}, _From, Ts) ->
    {reply, {error, badarg}, Ts, next_timeout()};
handle_call({apply_after, _, _}, _From, Ts) ->
    {reply, {error, badarg}, Ts, next_timeout()};
handle_call({apply_interval, _, _}, _From, Ts) ->
    {reply, {error, badarg}, Ts, next_timeout()};
handle_call(_Else, _From, Ts) ->		  % Catch anything else
    {noreply, Ts, next_timeout()}.

-spec handle_info(term(), timers()) -> {'noreply', timers(), timeout()}.
handle_info(timeout, Ts) ->                       % Handle timeouts
    Timeout = timer_timeout(system_time()),
    {noreply, Ts, Timeout};
handle_info({'EXIT',  Pid, _Reason}, Ts) ->       % Oops, someone died
    pid_delete(Pid),
    {noreply, Ts, next_timeout()};
handle_info(_OtherMsg, Ts) ->                     % Other Msg's
    {noreply, Ts, next_timeout()}.

-spec handle_cast(term(), timers()) -> {'noreply', timers(), timeout()}.
handle_cast(_Req, Ts) ->                          % Not predicted but handled
    {noreply, Ts, next_timeout()}.

-spec terminate(term(), _State) -> 'ok'.
terminate(_Reason, _State) ->
    ok.

-spec code_change(term(), State, term()) -> {'ok', State}.
code_change(_OldVsn, State, _Extra) ->
    %% According to the man for gen server no timer can be set here.
    {ok, State}.

%%
%% timer_timeout(SysTime)
%%
%% Apply and remove already timed-out timers. A timer is a tuple
%% {Time, BRef, Op, MFA}, where Time is in microseconds.
%% Returns {Timeout, Timers}, where Timeout is in milliseconds.
%%
timer_timeout(SysTime) ->
    TimerTab = get(?TIMER_TAB),
    case ets:first(TimerTab) of
        '$end_of_table' ->
            infinity;
        {Time, _Ref} when Time > SysTime ->
            Timeout = (Time - SysTime + 999) div 1000,
            %% Returned timeout must fit in a small int
            erlang:min(Timeout, ?MAX_TIMEOUT);
        Key ->
            case ets:lookup(TimerTab, Key) of
                [{Key, timeout, MFA}] ->
                    ets:delete(TimerTab,Key),
                    do_apply(MFA),
                    timer_timeout(SysTime);
                [{{Time, Ref}, Repeat = {repeat, Interv, To}, MFA}] ->
                    ets:delete(TimerTab,Key),
                    NewTime = Time + Interv,
                    %% Update the interval entry (last in table)
                    ets:insert(get(?INTERVAL_TAB),{{interval,Ref},{NewTime,Ref},To}),
                    do_apply(MFA),
                    ets:insert(TimerTab, {{NewTime, Ref}, Repeat, MFA}),
                    timer_timeout(SysTime)
            end
    end.

%%
%% delete_ref
%%

delete_ref(BRef = {interval, _}) ->
    IntervalTab = get(?INTERVAL_TAB),
    case ets:lookup(IntervalTab, BRef) of
        [{_, BRef2, _Pid}] ->
            ets:delete(IntervalTab, BRef),
            ets:delete(get(?TIMER_TAB), BRef2);
        _ -> % TimerReference does not exist, do nothing
            ok
    end;
delete_ref(BRef) ->
    ets:delete(get(?TIMER_TAB), BRef).

%%
%% pid_delete
%%

-spec pid_delete(pid()) -> 'ok'.
pid_delete(Pid) ->
    IntervalTab = get(?INTERVAL_TAB),
    IntervalTimerList =
        ets:select(IntervalTab,
            [{{'_', '_','$1'},
                [{'==','$1',Pid}],
                ['$_']}]),
    lists:foreach(fun({IntKey, TimerKey, _ }) ->
        ets:delete(IntervalTab, IntKey),
        ets:delete(get(?TIMER_TAB), TimerKey)
                  end, IntervalTimerList).

%% Calculate time to the next timeout. Returned timeout must fit in a
%% small int.

-spec next_timeout() -> timeout().
next_timeout() ->
    case ets:first(get(?TIMER_TAB)) of
        '$end_of_table' ->
            infinity;
        {Time, _} ->
            erlang:min(positive((Time - system_time() + 999) div 1000), ?MAX_TIMEOUT)
    end.

%% Help functions
do_apply({M,F,A}) ->
    case {M, F, A} of
        {?MODULE, send, A} ->
            %% If send op. send directly, (faster than spawn)
            catch send(A);
        {erlang, exit, [Name, Reason]} ->
            catch exit(get_pid(Name), Reason);
        _ ->
            %% else spawn process with the operation
            catch spawn(M,F,A)
    end.

positive(X) ->
    erlang:max(X, 0).


%%
%%  system_time() -> time in microseconds
%%
system_time() ->
    erlang:monotonic_time(1000000).

send([Pid, Msg]) ->
    Pid ! Msg.

get_pid(Name) when is_pid(Name) ->
    Name;
get_pid(undefined) ->
    undefined;
get_pid(Name) when is_atom(Name) ->
    get_pid(whereis(Name));
get_pid(_) ->
    undefined.

%%
%% get_status() ->
%%    {{TimerTabName,TotalNumTimers},{IntervalTabName,NumIntervalTimers}}
%%
%% This function is for test purposes only; it is used by the test suite.
%% There is a small possibility that there is a mismatch of one entry
%% between the 2 tables if this call is made when the timer server is
%% in the middle of a transaction

% -spec get_status() ->
% 	{{?TIMER_TAB,non_neg_integer()},{?INTERVAL_TAB,non_neg_integer()}}.

get_status() ->
    TimerTab = get(?TIMER_TAB),
    IntervalTab = get(?INTERVAL_TAB),
    Info1 = ets:info(TimerTab),
    {size,TotalNumTimers} = lists:keyfind(size, 1, Info1),
    Info2 = ets:info(IntervalTab),
    {size,NumIntervalTimers} = lists:keyfind(size, 1, Info2),
    {{TimerTab,TotalNumTimers},{IntervalTab,NumIntervalTimers}}.
