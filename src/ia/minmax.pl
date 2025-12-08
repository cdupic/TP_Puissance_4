minmaxIa(Board, Move, Player) :- % TODO: code the IA
    repeat,
    Column is random(7), % choose random column
    dropPiece(Board, Column, Move).


get3AlignedInRow(Board, Move, Player, Count) :-
    getRow(Board, Move, RowList),
    countTriples(RowList, Player, Count).

get3AlignedInColumn(Board, Move, Player, Count) :-
    getColumn(Board, Move, ColumnList),
    countTriples(ColumnList, Player, Count).


get3AlignedInDiagonalDownRight(Board, Move, Player, Count) :-
    getDiagonalDownRight(Board, Move, DiagonalList),
    countTriples(DiagonalList, Player, Count).

get3AlignedInDiagonalDownLeft(Board, Move, Player, Count) :-
    getDiagonalDownLeft(Board, Move, DiagonalList),
    countTriples(DiagonalList, Player, Count).



get2AlignedInRow(Board, Move, Player, Count) :-
    getRow(Board, Move, RowList),
    countTuples(RowList, Player, Count).


get2AlignedInColumn(Board, Move, Player, Count) :-
    getColumn(Board, Move, ColumnList),
    countTuples(ColumnList, Player, Count).


get2AlignedInDiagonalDownRight(Board, Move, Player, Count) :-
    getDiagonalDownRight(Board, Move, DiagonalList),
    countTuples(DiagonalList, Player, Count).

get2AlignedInDiagonalDownLeft(Board, Move, Player, Count) :-
    getDiagonalDownLeft(Board, Move, DiagonalList),
    countTuples(DiagonalList, Player, Count).



% Renvoie la valeur de la case Index dans Board ; remplace une variable par 'E'
cell_value(Board, Index, ValOut) :-
    nth0(Index, Board, Val),
    ( var(Val) -> ValOut = 'E' ; ValOut = Val ).

getColumn(Board, Move, ColumnList) :-
    Col is Move mod 7,
    findall(Val,
            ( between(0,5,Row),
              Index is Row*7 + Col,
              cell_value(Board, Index, Val)
            ),
            ColumnList).

getRow(Board, Move, RowList) :-
    Row is Move // 7,
    Start is Row * 7,
    findall(Val,
            ( between(0,6,Off),
              Index is Start + Off,
              cell_value(Board, Index, Val)
            ),
            RowList).

getDiagonalDownRight(Board, Move, DiagonalList) :-
    getTopLeft(Move, Start),
    findall(Val,
            ( between(0,5,Step),
              Cell is Start + 8*Step,
              Cell =< 41,
              Col is Cell mod 7,
              Col \= 6,
              cell_value(Board, Cell, Val)
            ),
            DiagonalList).

getDiagonalDownLeft(Board, Move, DiagonalList) :-
    getTopRight(Move, Start),
    findall(Val,
            ( between(0,5,Step),
              Cell is Start + 6*Step,
              Cell =< 41,
              Col is Cell mod 7,
              Col \= 0,
              cell_value(Board, Cell, Val)
            ),
            DiagonalList).

% ------------------- TRIPLES -----------------------

countTriples(List, Player, Count) :-
    countTriplesAux(List, Player, 0, Count).

countTriplesAux([Player,Player,Player|Rest], Player, Acc, Count) :-
    Acc1 is Acc + 1,
    countTriplesAux(Rest, Player, Acc1, Count).

countTriplesAux([_|Rest], Player, Acc, Count) :-
    countTriplesAux(Rest, Player, Acc, Count).

countTriplesAux([], _, Count, Count).

% ------------------- TUPLES -----------------------
countTuples(List, Player, Count) :-
    countTuplesAux(List, Player, 0, Count).

countTuplesAux([Player,Player|Rest], Player, Acc, Count) :-
    Acc1 is Acc + 1,
    countTuplesAux(Rest, Player, Acc1, Count).

countTuplesAux([_|Rest], Player, Acc, Count) :-
    countTuplesAux(Rest, Player, Acc, Count).

countTuplesAux([], _, Count, Count).

getWeightPosition(Board, Move, Player, Weight) :-
    % On supppose que le coup a été simulé avant (j'ai pas réussi à le simuler en phase de test)
    % on considère égalements toutes les positions sur les lignes/colonnes et diagonnales associées au move,
    % pas seulement celles qui sont adjacentes au ooint qui vient d'être joué.

    ( gameOver(Board, Move, _) ->
        Weight = 100000
    ;

        get3AlignedInRow(Board, Move, Player, C1R),
        get3AlignedInColumn(Board, Move, Player, C1C),
        get3AlignedInDiagonalDownRight(Board, Move, Player, C1DR),
        get3AlignedInDiagonalDownLeft(Board, Move, Player, C1DL),
        Score3 is (C1R + C1C + C1DR + C1DL) * 100,

        ( Score3 > 0 ->
            Weight = Score3
        ;
            get2AlignedInRow(Board, Move, Player, C2R),
            get2AlignedInColumn(Board, Move, Player, C2C),
            get2AlignedInDiagonalDownRight(Board, Move, Player, C2DR),
            get2AlignedInDiagonalDownLeft(Board, Move, Player, C2DL),
            Score2 is (C2R + C2C + C2DR + C2DL) * 10,
            Weight = Score2
        )
    ), !.


max(X1,X2,Max) :-
    X1 > X2 -> Max = X1;
    Max = X2.

min(X1,X2,Min) :-
    X1 > X2 -> Min = X2;
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
                R is R1 + 1.
