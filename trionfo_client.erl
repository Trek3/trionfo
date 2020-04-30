-module(trionfo_client).
-export([start_client/1, start_game/1, exit_game/1, accept_game/1]).

start_client(Node) -> register(player, spawn(node(), fun() -> connect(Node) end)).

connect(Node) -> 
    {server, Node} ! {new, {player, node()}},
    clientLoop().

clientLoop() ->
    receive 
        {connected, Node} ->
            io:format("You successfully joined the game of ~p!~n", [Node]),
            clientLoop();
        {error, full} ->
            io:format("The room is full already! Try again later!~n"),
            clientLoop();
        {error, not_a_member} ->
            io:format("You can't start a game because you are not in the room!~n"),
            clientLoop();
        {error, four_players} ->
            io:format("You can't start a game because there are not 4 players in the room"),
            clientLoop();
        {start_game, Other} ->
            io:format("~p wants to play! Ok?~n", [Other]),
            clientLoop();
        {ok, start_game} ->
            io:format("Game is about to start!"),
            clientLoop();
        _ -> 
            clientLoop()
    end.

accept_game(Node) ->
    {server, Node} ! {yes_start_game, {player, node()}}.

start_game(Node) ->
    {server, Node} ! {start_game, {player, node()}}.

exit_game(Node) ->
    unregister(player),
    {server, Node} ! {exit, {player, node()}}.
    