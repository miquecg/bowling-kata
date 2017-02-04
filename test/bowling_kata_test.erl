-module(bowling_kata_test).

-include_lib("eunit/include/eunit.hrl").


start() ->
    {ok, PID} = bowling_game:new(),
    PID.

stop(PID) ->
    bowling_game:stop(PID).

rolls_without_bonuses_test_() ->
    {foreach,
        fun start/0,
        fun stop/1,
        [fun(PID) -> ?_assertEqual(0, do_rolls(lists:duplicate(20, 0), PID)) end]
    }.

do_rolls(Rolls, PID) ->
    lists:foldl(fun(KnockedPins, _Points) -> bowling_game:roll(PID, KnockedPins) end, 0, Rolls).
