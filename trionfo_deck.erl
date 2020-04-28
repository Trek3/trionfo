-module(trionfo_deck).
-export([prepare_deck/0]).

gen() -> [X || X <- ["1B", "2B", "3B", "4B", "5B", "6B", "7B", "FB", "CB", "RB",
                    "1C", "2C", "3C", "4C", "5C", "6C", "7C", "FC", "CC", "RC",
                    "1D", "2D", "3D", "4D", "5D", "6D", "7D", "FD", "CD", "RD",
                    "1S", "2S", "3S", "4S", "5S", "6S", "7S", "FS", "CS", "RS"]].

prepare_deck() ->
    D = gen(),
    shuffle(D).

shuffle(X) -> [Y||{_,Y} <- lists:sort([ {rand:uniform(), N} || N <- X])].