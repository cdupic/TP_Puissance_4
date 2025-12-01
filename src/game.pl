:- dynamic board/1.     % permet l'assertion et le retrait de faits board/1

% Affiche le plateau (42 cases = 6 lignes × 7 colonnes)
displayBoard(Board) :-
    between(0, 41, X),
    writeElem(X, Board),
    checkline(X),
    X == 41.

checkline(X) :-
    ( X == 6 ; X == 13 ; X == 20 ; X == 27 ; X == 34 ; X == 41 ),
    writeln('').

writeElem(X, Board) :-
    nth0(X, Board, Z),
    % if Z == 'X' then print " X ", else if Z == 'Y' then print " Y ", else print " . "
    (Z == 'X' -> format(" X ");
    Z == 'Y' -> format(" Y ");
    format(" . ")).


init :-
    length(Board, 42),
    assert(board(Board)).
