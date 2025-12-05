:- dynamic board/1.     % permet l'assertion et le retrait de faits board/1
:- dynamic game_number/1.

% Affiche le plateau (42 cases = 6 lignes × 7 colonnes)
% 0-1-2-3-4-5-6
% 7-8-9-10-11-12-13
% 35-36-37-38-39-40-41
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
    (Z == 'X' -> format("🟡");
    Z == 'Y' -> format("🔴");
    format("..")).

changePlayer('X','Y').
changePlayer('Y','X').


ia(Board, Move, Player) :-
    repeat,
    Column is random(7), % choose random column
    dropPiece(Board, Column, Move).

dropPiece(Board, Column, Move) :-
    between(0, 6, Row),
    Move is 35 + Column - 7*Row,
    nth0(Move, Board, Val),
    var(Val).

playMove(Board,Move,NewBoard,Player) :-
		copy_term(Board, NewBoard),
    	nth0(Move, NewBoard,Player).


applyIt(Board, NewBoard) :-
    retract(board(Board)), % on oublie l'ancienne règle qui n'est plus vraie
    assert(board(NewBoard)).


linecompleted(Board, Move) :-
    retractall(game_number(_)),      % compteur à 0 au début
    assert(game_number(0)),

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
    retractall(game_number(_)),      % compteur à 0 au début
    assert(game_number(0)),

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
    MoveCol is Move mod 7,
    between(0, 5, R),
    TopRight is Move - 6*R,
    TopRightCol is TopRight mod 7,
    (TopRightCol == 6;  % si on arrive à la dernière colonne, exit
    TopRight < 7),  % ou on est sur la première ligne
    !.


getTopLeft(Move, TopLeft) :-
    MoveCol is Move mod 7,
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






incr :-
    retract(game_number(N)),
    N1 is N + 1,
    assert(game_number(N1)).


reset_counter :-
    retract(game_number(_)),
    assert(game_number(0)).


%gameOver(Board, Move) :-   % first check if board is full
%    write("Draw\n"),
%    between(0, 41, X),
%    nth0(X, Board, Val),
%    nonvar(Val).

gameOver(Board, Move) :- % else check if move is winning move
    % check if a line as been completed
    ( linecompleted(Board, Move) -> format("line completed at move~w~n", [Move]) ;
    % check if a column has been completed
    columncompleted(Board, Move) -> format("column completed at move~w~n", [Move]));
    % check if a diagonal has been completed
    (diagonalcompleted(Board, Move) -> format("diagonal completed at move~w~n", [Move])) .



play(Player):-  write('New turn for:'), writeln(Player),
    		board(Board), % instanciate the board from the knowledge base
       	    displayBoard(Board), % print it
            ia(Board, Move,Player), % ask the AI for a move, that is, an index for the Player
    	    playMove(Board,Move,NewBoard,Player), % Play the move and get the result in a new Board
		    applyIt(Board, NewBoard), % Remove the old board from the KB and store the new one
		    ( gameOver(NewBoard, Move) ->
                    write("Game over !!\n"),
                    displayBoard(NewBoard), !
                ;
                    changePlayer(Player, NextPlayer),
                    play(NextPlayer)
            ).

init :-
    length(Board, 42),
    assert(board(Board)),
    play('X').
