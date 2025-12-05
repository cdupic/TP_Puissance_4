linecompleted(Board, Move) :-
    reset_counter,
    nth0(Move, Board, Player),
    %format("[linecompleted] Move=~w Player=~w~n", [Move, Player]),

    Row is Move // 7,
    %format("  Row(computed)=~w~n", [Row]),

    between(0, 6, Column),

    Cell is Row*7 + Column,
    %format("  -> Column=~w CellIndex=~w~n", [Column, Cell]),

    nth0(Cell, Board, Val),
    %format("     CellVal=~w (vs Player=~w)~n", [Val, Player]),

    ( Val == Player -> incr ; reset_counter ),

    game_number(N),
    %format("  game_number(after update)=~w~n", [N]),
    N >= 4, !.


columncompleted(Board, Move) :-
    reset_counter,

    nth0(Move, Board, Player),
    %format("[columncompleted] Move=~w Player=~w~n", [Move, Player]),

    Column is mod(Move,7),
    %format("  Column(computed)=~w~n", [Column]),

    between(0, 5, Row),

    Cell is Row*7 + Column,
    %format("  -> Column=~w CellIndex=~w~n", [Column, Cell]),

    nth0(Cell, Board, Val),
    %format("     CellVal=~w (vs Player=~w)~n", [Val, Player]),

    ( Val == Player -> incr ; reset_counter ),

    game_number(N),
    %format("  game_number(after update)=~w~n", [N]),
    N >= 4, !.

getTopRight(Move, TopRight) :-
    between(0, 5, R),
    TopRight is Move - 6*R,
    TopRightCol is TopRight mod 7,
    (TopRightCol == 6;  % si on arrive à la dernière colonne, exit
    TopRight < 7),  % ou on est sur la première ligne
    !.


getTopLeft(Move, TopLeft) :-
    between(0, 5, R),
    TopLeft is Move - 8*R,
    TopLeftCol is TopLeft mod 7,
    (TopLeftCol == 0 ; % si on arrive à la dernière colonne, exit
    TopLeft < 7 ), % si on est sur la première ligne, exit
    !.


topLeftDiagonal(Board, Move) :-
    retractall(game_number(_)),
    assert(game_number(0)),

    nth0(Move, Board, Player),
    %format("[topLeftDiagonal] Move=~w Player=~w~n", [Move, Player]),

    % on récupère la case le plus en haut à gauche de cette diagonale puis traverse jusqu'à la case le plus en bas à droite
    getTopLeft(Move, TopLeft),

    between(0, 5, Row),

    Cell is TopLeft + 8*Row,
    Cell =< 41,
    MoveCol is Cell mod 7,
    %format(" CellIndex=~w~n", [Cell]),

    nth0(Cell, Board, Val),
    %format("     CellVal=~w (vs Player=~w)~n", [Val, Player]),
    ( Val == Player -> incr ; reset_counter ),
    game_number(N),
    %format("game_number(after update)=~w~n", [N]),
    (  N >= 4 -> !, true
    ;  MoveCol == 6 -> !, false
    ),
         !.


topRightDiagonal(Board, Move) :-
    retractall(game_number(_)),
    assert(game_number(0)),

    nth0(Move, Board, Player),
    %format("[topRightDiagonal] Move=~w Player=~w~n", [Move, Player]),

    % on récupère la case le plus en haut à gauche de cette diagonale puis traverse jusqu'à la case le plus en bas à droite
    getTopRight(Move, TopRight),

    between(0, 5, Row),

    Cell is TopRight + 6*Row,
    Cell =< 41,
    MoveCol is Cell mod 7,

    %format(" CellIndex=~w~n", [Cell]),

    nth0(Cell, Board, Val),
    %format("     CellVal=~w (vs Player=~w)~n", [Val, Player]),

    ( Val == Player -> incr ; reset_counter ),

    game_number(N),
    %format("game_number(after update)=~w~n", [N]),
    (  N >= 4 -> !, true
        ;  MoveCol == 0 -> !, false
        ),
    !.

diagonalcompleted(Board, Move) :-
    topLeftDiagonal(Board, Move);
    topRightDiagonal(Board, Move).

%gameOver(Board, Move) :-   % first check if board is full
%    write("Draw\n"),
%    between(0, 41, X),
%    nth0(X, Board, Val),
%    nonvar(Val).


gameOver(Board, Move, Details):-
    linecompleted(Board, Move) -> Details = "Line completed";
    columncompleted(Board, Move) -> Details = "Column completed";
    diagonalcompleted(Board, Move) -> Details = "Diagonal completed".

incr:-
    retract(game_number(N)),
    N1 is N + 1,
    assert(game_number(N1)).

reset_counter:-
    retractall(game_number(_)),
    assert(game_number(0)).
