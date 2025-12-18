

neuralNetworkAiReLu(Board, Move, Player) :-
    formatBoardForNN(Board, Player, Str),
    %writeln(Str),
    format(string(Command), ".\\move_calculation-ReLu.exe ~w", [Str]),
    %writeln(Command),
    shell(Command, Column),
    %writeln(Column),
    dropPiece(Board, Column, Move)
    %writeln(Move)
.