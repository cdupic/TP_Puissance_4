ia(Board, Move, Player) :- % TODO: code the IA
    repeat,
    Column is random(7), % choose random column
    dropPiece(Board, Column, Move).


max(X1,X2,Max) :-
    X1 > X2 -> Max = X1;
    Max = X2.

min(X1,X2,Min) :-
    X1 > X2 -> Max = X2;
    Min = X1.

minimax(Depth, NodeIndex, _IsMax, Scores, H, OptimalValue) :-
    Depth =:= H,
    nth0(NodeIndex, Scores, OptimalValue), !.

minimax(Depth, NodeIndex, IsMax, Scores, H, OptimalValue) :-
    NewDepth is Depth + 1,
    NewNodeIndex1 is NodeIndex * 2,
    NewNodeIndex2 is NewNodeIndex1 + 1,
    ( IsMax =:= 1 ->
        minimax(NewDepth, NewNodeIndex1, 0, Scores, H, N1),
        minimax(NewDepth, NewNodeIndex2, 0, Scores, H, N2),
        max(N1, N2, OptimalValue)
    ;
        minimax(NewDepth, NewNodeIndex1, 1, Scores, H, N1),
        minimax(NewDepth, NewNodeIndex2, 1, Scores, H, N2),
        min(N1, N2, OptimalValue)
    ).



logBase2(1,0).
logBase2(N,R) :-
                N>1,
                N1 is N//2,
                logBase2(N1,R1),
                R is R1 +1.
