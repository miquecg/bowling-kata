-module(bowling_game).

%% API
-export([
    new/0,
    roll/2,
    stop/1
]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).


%%===================================================================
%% API
%%===================================================================

-spec new() ->
    {ok, pid()} | ignore | {error, Error :: {already_started, pid()} | term()}.
new() ->
    gen_server:start(?MODULE, [], []).

-spec roll(PID :: pid(), KnockedPins :: 0..10) ->
    0..300.
roll(PID, KnockedPins) ->
    gen_server:call(PID, {roll, KnockedPins}).

-spec stop(PID :: pid()) ->
    ok.
stop(PID) ->
    ExitReason = normal,
    TimeoutInMilliseconds = 2000,
    gen_server:stop(PID, ExitReason, TimeoutInMilliseconds).


%%===================================================================
%% gen_server callbacks
%%===================================================================

init([]) ->
    {ok, 0}.

handle_call({roll, KnockedPins}, _From, State) ->
    Points = State + KnockedPins,
    {reply, Points, Points};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Message, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVersion, State, _Extra) ->
    {ok, State}.
