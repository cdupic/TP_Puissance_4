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

neuralNetworkAiReLu(Board, Move, Player) :-
    formatBoardForNN(Board, Player, Str),
    writeln(Str),
    format(string(Command), "C:\\Users\\aurel\\Desktop\\Cours\\4A\\ALIA\\TP_Puissance_4\\src\\Neural-Network\\move_calculation-ReLu.exe ~w", [Str]),
    writeln(Command),
    shell(Command, Column),
    writeln(Column),
    dropPiece(Board, Column, Move),
    writeln(Move)
.