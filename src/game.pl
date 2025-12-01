:- dynamic board/1.     % permet l'assertion et le retrait de faits board/1

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
    (Z == 'X' -> format(" X ");
    Z == 'Y' -> format(" Y ");
    format(" . ")).

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


play(Player):-  write('New turn for:'), writeln(Player),
    		board(Board), % instanciate the board from the knowledge base
       	    displayBoard(Board), % print it
            ia(Board, Move,Player), % ask the AI for a move, that is, an index for the Player
    	    playMove(Board,Move,NewBoard,Player), % Play the move and get the result in a new Board
		    applyIt(Board, NewBoard), % Remove the old board from the KB and store the new one
    	    changePlayer(Player,NextPlayer), % Change the player before next turn
            play(NextPlayer). % next turn!

init :-
    length(Board, 42),
    assert(board(Board)),
    play('X').
