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
-type roll() :: 0..10.
-type score() :: 0..300.

-record(state, {
    score = 0 :: score(),
    frame = [] :: [roll()],
    bonus = [] :: [bonus()]
}).
-type game() :: #state{}.


%%===================================================================
%% API
%%===================================================================

-spec new() ->
    {ok, pid()} | ignore | {error, Error :: {already_started, pid()} | term()}.
new() ->
    gen_server:start(?MODULE, [], []).

-spec roll(PID :: pid(), KnockedPins :: roll()) ->
    score().
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

handle_call({roll, KnockedPins}, _From, Game) ->
    NewGame1 = update_score(KnockedPins, Game),
    NewGame2 = update_frame(KnockedPins, NewGame1),
    NewGame = update_bonus(NewGame2),
    {reply, NewGame#state.score, NewGame};
handle_call(_Request, _From, Game) ->
    {reply, ok, Game}.

handle_cast(_Request, Game) ->
    {noreply, Game}.

handle_info(_Message, Game) ->
    {noreply, Game}.

terminate(_Reason, _Game) ->
    ok.

code_change(_OldVersion, Game, _Extra) ->
    {ok, Game}.


%%===================================================================
%% Internal functions
%%===================================================================

-spec update_score(KnockedPins :: roll(), Game :: game()) ->
    game().
update_score(KnockedPins, #state{bonus = [], score = Score} = Game) ->
    Game#state{score = Score + KnockedPins};
update_score(KnockedPins, #state{bonus = [x2 | NextBonuses], score = Score} = Game) ->
    BonusedRoll = KnockedPins * 2,
    Game#state{
        score = Score + BonusedRoll,
        bonus = NextBonuses
    }.

-spec update_frame(KnockedPins :: roll(), Game :: game()) ->
    game().
update_frame(KnockedPins, #state{frame = [_, _]} = Game) ->
    Game#state{frame = [KnockedPins]};
update_frame(KnockedPins, #state{frame = [10]} = Game) ->
    Game#state{frame = [KnockedPins]};
update_frame(KnockedPins, #state{frame = Frame} = Game) when is_list(Frame) ->
    Game#state{frame = [KnockedPins | Frame]}.

-spec update_bonus(Game :: game()) ->
    game().
update_bonus(#state{frame = [SecondRoll, FirstRoll], bonus = Bonus} = Game) when SecondRoll + FirstRoll =:= 10 ->
    Game#state{bonus = lists:append(Bonus, [x2])};
update_bonus(#state{frame = [10], bonus = Bonus} = Game) ->
    Game#state{bonus = lists:append(Bonus, [x2, x2])};
update_bonus(#state{} = Game) ->
    Game.
