
validInput(0).
validInput(1).
validInput(2).
validInput(3).
validInput(4).
validInput(5).
validInput(6).


userAi(Board, Move, _) :-
    % ask user for input
    repeat,
        writeln("Enter a valid move (0-6) :"),
        read(UserColumn),
        validInput(UserColumn),
        dropPiece(Board, UserColumn, UserMove),
    !,
    dropPiece(Board, UserColumn, Move).

    




