:- dynamic str/1.

buildPlayerBoard(Board, X, Player):-
    str(Str),
    nth0(X,Board,Cell),
    (var(Cell) -> string_concat(Str, "0", Res); true),
    ((nonvar(Cell), Cell==Player) -> string_concat(Str, "1", Res); string_concat(Str, "0", Res)),
    retract(str(Str)),
    assert(str(Res)),
    not(X == 41) -> ((Y is X+1, buildPlayerBoard(Board, Y, Player)); true)
.

formatBoardForNN(Board, Player, Str) :-
    changePlayer(Player, Opponent),
    (str(OldStr) -> retract(str(OldStr)); true),
    NewStr = "", assert(str(NewStr)),
    buildPlayerBoard(Board, X, Player),
    buildPlayerBoard(Board, X, Opponent),
    str(Str)
.

neuralNetworkAi(Board, Move, Player) :-
    formatBoardForNN(Board, Player, Str),
    format(Command, "C:/Users/aurel/Desktop/Neural-Network/move_calculation.exe ~w", [Str])
    shell(Command, Move)
.