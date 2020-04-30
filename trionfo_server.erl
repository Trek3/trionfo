-module(trionfo_server).
-export([start_server/0, members/1]).
-import(trionfo_deck, [prepare_deck/0]).

start_server() -> register(server, spawn( fun() -> process_flag(trap_exit, true), serverLoop([], false, 0) end)).

serverLoop(P, GameStarted, PlayersReady) ->
    receive
        {get_players} -> P;
        {new, From} -> 
            {_Pid, Node} = From,
            case length(P) of
                Count when Count < 4 -> 
                    P1 = [From | P],
                    io:format("~p connected to the game!~n", [Node]),
                    From ! {connected, node()},
                    members(P1),
                    serverLoop(P1, false, 0);
                _ -> 
                    From ! {error, full},
                    serverLoop(P, GameStarted, 0)
                end;
        {exit, From} ->
            {_Pid, Node} = From,
            P1 = remove(From, P),
            io:format("~p exited the game!~n", [Node]),
            members(P1),
            serverLoop(P1, false, 0);
        {start_game, From} when length(P) == 4->
            {_Pid, Node} = From,
            case lookup(From, P) of
                ok ->
                    send_message(From, P, {start_game, Node}),
                    serverLoop(P, true, 1);
                error ->
                    From ! {error, not_a_member},
                    serverLoop(P, false, 0)
                end;
        {start_game, From} when length(P) /= 4 -> 
            From ! {error, four_players};
        {yes_start_game, From} when GameStarted == true, PlayersReady == 3 ->
            case lookup(From, P) of
                ok ->
                    send_message({nopid, nonode}, P, {ok, start_game}),
                    serverLoop(P, GameStarted, PlayersReady + 1);
                error ->
                    From ! {error, not_a_member}
                end;
        {yes_start_game, From} when GameStarted == true ->
            case lookup(From, P) of
                ok ->
                    serverLoop(P, GameStarted, PlayersReady + 1);
                error ->
                    From ! {error, not_a_member}
                end;
        {yes_start_game, _From} when GameStarted == false ->
            serverLoop(P, GameStarted, PlayersReady)
        end.
        
lookup({_Pid, Node},[{_, Node}|_]) -> ok;
lookup(G,[_|T]) -> lookup(G, T);
lookup(_,[]) -> error.

send_message(_From, [], Msg) -> io:format("Message ~p sent to all the players!~n", [Msg]);
send_message({Pid, Node}, [{_, Node} | P], Msg) -> send_message({Pid, Node}, P, Msg);
send_message(From, [Else | P], Msg) -> Else ! Msg, send_message(From, P, Msg).

remove({_Pid, Node}, [{_Pid, Node}|P]) -> P;
remove(G, [H|P]) -> [H|remove(G, P)];
remove(_, []) -> [].

members(P) -> print_connected(P, 0).

print_connected([], 0) -> io:format("The room is empty.");
print_connected([], Total) -> io:format("There are ~p players connected at the moment.", [Total]); 
print_connected([ {_, Node} | P ], Total) ->
    io:format("(~p)~n", [Node]),
    print_connected(P, Total+1).