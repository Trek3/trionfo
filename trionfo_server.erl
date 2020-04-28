-module(trionfo_server).
-export([start_server/0, members/1]).
-import(trionfo_deck, [prepare_deck/0]).

start_server() -> register(server, spawn( fun() -> serverLoop([]) end)).

serverLoop(P) ->
    receive
        {get_players} -> P;
        {new, User, From} -> 
            case length(P) of
                Count when Count < 4 -> 
                    P1 = [{User, From} | P],
                    io:format("~p connected to the game!~n", [User]),
                    From ! {connected, node()},
                    members(P1),
                    serverLoop(P1);
                _ -> 
                    From ! {error, full},
                    serverLoop(P)
                end;
        {exit, User, From} ->
            P1 = remove({User, From}, P),
            io:format("~p exited the game!~n", [User]),
            members(P1),
            serverLoop(P1);
        {start_game, User, From} ->
            case length(P) of
                4 ->
                    case lookup({User, From}, P) of
                        ok ->
                            send_message(From, P, {start_game, User});
                        error ->
                            From ! {error, not_a_member}
                            end,
                    serverLoop(P);
                _ -> 
                    From ! {error, four_players}
                end
        end.
        
lookup(G,[{G,_Pid}|_]) -> ok;
lookup(G,[_|T]) -> lookup(G, T);
lookup(_,[]) -> error.

send_message(_Pid, [], Msg) -> io:format("Message ~p sent to all the players!~n", [Msg]);
send_message(Pid, [{_Else, Pid} | P], Msg) -> Pid ! Msg, send_message(Pid, P, Msg);
send_message(Pid, [{_User, _Other} | P], Msg) -> send_message(Pid, P, Msg).

remove(G, [G|P]) -> P;
remove(G, [H|P]) -> [H|remove(G, P)];
remove(_, []) -> [].

members(P) -> print_connected(P, 0).

print_connected([], 0) -> io:format("The room is empty.");
print_connected([], Total) -> io:format("There are ~p players connected at the moment.", [Total]); 
print_connected([ {User, {_, Node}} | P ], Total) ->
    io:format("~p -> (~p)~n", [User, Node]),
    print_connected(P, Total+1).