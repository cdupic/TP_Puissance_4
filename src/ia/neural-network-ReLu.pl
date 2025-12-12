

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