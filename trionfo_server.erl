-module(trionfo_server).
-export([start_server/0, members/1]).
-import(trionfo_deck, [prepare_deck/0]).

start_server() -> register(server, spawn( fun() -> process_flag(trap_exit, true), serverLoop([], false, 0) end)).

serverLoop(P, GameStarted, PlayersReady) ->
    receive
        {get_players} -> P;
        {new, User, From} -> 
            case length(P) of
                Count when Count < 4 -> 
                    P1 = [{User, From} | P],
                    io:format("~p connected to the game!~n", [User]),
                    From ! {connected, node()},
                    members(P1),
                    serverLoop(P1, false, 0);
                _ -> 
                    From ! {error, full},
                    serverLoop(P, GameStarted, 0)
                end;
        {exit, User, From} ->
            P1 = remove({User, From}, P),
            io:format("~p exited the game!~n", [User]),
            members(P1),
            serverLoop(P1, false, 0);
        {start_game, User, From} when length(P) == 4->
            case lookup({User, From}, P) of
                ok ->
                    send_message(From, P, {start_game, User}),
                    serverLoop(P, true, 1);
                error ->
                    From ! {error, not_a_member},
                    serverLoop(P, false, 0)
                end;
        {start_game, _User, From} when length(P) /= 4 -> 
            From ! {error, four_players};
        {yes_start_game, User, From} when GameStarted == true, PlayersReady == 3 ->
            case lookup({User, From}, P) of
                ok ->
                    send_message({nopid, nonode}, P, {ok, start_game}),
                    serverLoop(P, GameStarted, PlayersReady + 1);
                error ->
                    From ! {error, not_a_member}
                end;
        {yes_start_game, User, From} when GameStarted == true ->
            case lookup({User, From}, P) of
                ok ->
                    serverLoop(P, GameStarted, PlayersReady + 1);
                error ->
                    From ! {error, not_a_member}
                end;
        {yes_start_game, _User, _From} when GameStarted == false ->
            serverLoop(P, GameStarted, PlayersReady)
        end.
        
lookup(G,[{G,_Pid}|_]) -> ok;
lookup(G,[_|T]) -> lookup(G, T);
lookup(_,[]) -> error.

send_message(_Pid, [], Msg) -> io:format("Message ~p sent to all the players!~n", [Msg]);
send_message(Pid, [{_User, _Other} | P], Msg) -> Pid ! Msg, send_message(Pid, P, Msg);
send_message(Pid, [{_Else, Pid} | P], Msg) -> send_message(Pid, P, Msg).

remove(G, [G|P]) -> P;
remove(G, [H|P]) -> [H|remove(G, P)];
remove(_, []) -> [].

members(P) -> print_connected(P, 0).

print_connected([], 0) -> io:format("The room is empty.");
print_connected([], Total) -> io:format("There are ~p players connected at the moment.", [Total]); 
print_connected([ {User, {_, Node}} | P ], Total) ->
    io:format("~p -> (~p)~n", [User, Node]),
    print_connected(P, Total+1).