randomIa(Board, Move, Player) :-
    repeat,
    Column is random(7), % choose random column
    dropPiece(Board, Column, Move).
