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

%% Types
-type bonus() :: x2.

-record(state, {
    score = 0 :: 0..300,
    frame = [],
    bonus = [] :: [bonus()]
}).


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
    {ok, #state{}}.

handle_call({roll, KnockedPins}, _From, State) ->
    NewState1 = update_score(KnockedPins, State),
    NewState2 = update_frame(KnockedPins, NewState1),
    NewState = update_bonus(NewState2),
    {reply, NewState#state.score, NewState};
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


%%===================================================================
%% Internal functions
%%===================================================================

-spec update_score(KnockedPins :: 0..10, State :: #state{}) ->
    #state{}.
update_score(KnockedPins, #state{bonus = [], score = Score} = State) ->
    State#state{score = Score + KnockedPins};
update_score(KnockedPins, #state{bonus = [x2 | NextBonuses], score = Score} = State) ->
    BonusedRoll = KnockedPins * 2,
    State#state{
        score = Score + BonusedRoll,
        bonus = NextBonuses
    }.

-spec update_frame(KnockedPins :: 0..10, State :: #state{}) ->
    #state{}.
update_frame(KnockedPins, #state{frame = [_, _]} = State) ->
    State#state{frame = [KnockedPins]};
update_frame(KnockedPins, #state{frame = Frame} = State) when is_list(Frame) ->
    State#state{frame = [KnockedPins | Frame]}.

-spec update_bonus(State :: #state{}) ->
    #state{}.
update_bonus(#state{frame = [SecondRoll, FirstRoll], bonus = Bonus} = State) when SecondRoll + FirstRoll =:= 10 ->
    State#state{bonus = lists:append(Bonus, [x2])};
update_bonus(#state{} = State) ->
    State.
