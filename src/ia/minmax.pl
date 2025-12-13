minmaxIa(Board, Move, Player) :- % TODO: code the IA
    repeat,
    Column is random(7), % choose random column
    dropPiece(Board, Column, Move).


get3AlignedInRow(Board, Move, Player, Count) :-
    getRow(Board, Move, RowList),
    write(RowList),
    countTriples(RowList, Player, Count).

get3AlignedInColumn(Board, Move, Player, Count) :-
    getColumn(Board, Move, ColumnList),
    countTriples(ColumnList, Player, Count).


get3AlignedInDiagonalDownRight(Board, Move, Player, Count) :-
    getDiagonalDownRight(Board, Move, DiagonalList),
        write(DiagonalList),
    countTriples(DiagonalList, Player, Count).

get3AlignedInDiagonalDownLeft(Board, Move, Player, Count) :-
    getDiagonalDownLeft(Board, Move, DiagonalList),
    write(DiagonalList),
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
free_cell(Val) :-
    Val == 'E'.

countTriplesNew(List, Player, Acc, Count) :-
    append(Prefix, [A,B,C|Suffix], List), % le append découpe suivant toutes les possibilités donc on teste bien toous les A,B,C possibles
    A == Player, B == Player, C == Player,
    write(Prefix), writeln(""), write([A,B,C|Suffix]), writeln(""),
    % Succeeds when Prev is the last element of Prefix -> on regarde si la dernière case du préfix est libre
    (( Prefix \= [], last(Prefix, Prev), free_cell(Prev) )
    ;  % sinon on regarde si la première case de suffix est libre
    ( Suffix \= [], Suffix = [Next|_], free_cell(Next) )),
    !, % empêche de retrouver le meme triple
    Acc1 is Acc + 1,
    countTriplesNew(Suffix, Player, Acc1, Count).

countTriplesNew(_, _, Count, Count).

countTriples(List, Player, Count) :-
    countTriplesNew(List, Player, 0, Count).




% ------------------- TUPLES -----------------------



countTuples(List, Player, Count) :-
    countTuplesAux(List, Player, 0, Count).

countTuplesAux(List, Player, Acc, Count) :-
    append(Prefix, [A,B|Suffix], List),
    A == Player,
    B == Player,

    free_left(Prefix, L),
    free_right(Suffix, R),

    L + R >= 2,

    !,
    Acc1 is Acc + 1,
    countTuplesAux(Suffix, Player, Acc1, Count).

countTuplesAux(_, _, Count, Count).

free_left(Prefix, Count) :-
    reverse(Prefix, Rev), % on inverse la liste dans prefix
    free_run(Rev, Count).

free_right(Suffix, Count) :-
    free_run(Suffix, Count).

free_run([], 0).
free_run(['E'|Rest], Count) :-
    free_run(Rest, C1),
    Count is C1 + 1.

free_run([H|_], 0) :-
    H \= 'E'.
% -----------------------------------------------------------------

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
