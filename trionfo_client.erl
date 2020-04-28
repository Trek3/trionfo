-module(trionfo_client).
-export([start_client/2, start_game/2, exit_game/2]).

start_client(Node, User) -> register(player, spawn(node(), fun() -> connect(Node, User) end)).

connect(Node, User) -> 
    {server, Node} ! {new, User, {player, node()}},
    clientLoop(User).

clientLoop(User) ->
    receive 
        {connected, Node} ->
            io:format("You successfully joined the game of ~p!~n", [Node]),
            clientLoop(User);
        {error, full} ->
            io:format("The room is full already! Try again later!~n"),
            clientLoop(User);
        {error, not_a_member} ->
            io:format("You can't start a game because you are not in the room!~n"),
            clientLoop(User);
        {start_game, Other} ->
            io:format("~p wants to play! Ok?~n", [Other]);  
        _ -> 
            clientLoop(User)
    end.

start_game(User, Node) ->
    {server, Node} ! {start_game, User, {player, node()}}.

exit_game(User, Node) ->
    unregister(player),
    {server, Node} ! {exit, User, {player, node()}}.
    