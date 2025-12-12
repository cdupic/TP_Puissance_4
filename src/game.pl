:- dynamic board/1.     % permet l'assertion et le retrait de faits board/1
:- dynamic game_number/1.

% Affiche le plateau (42 cases = 6 lignes × 7 colonnes)
% 0-1-2-3-4-5-6
% 7-8-9-10-11-12-13
% 35-36-37-38-39-40-41

displayBoard(Board) :-
    displayMode("heavy") -> displayBoardHeavy(Board) ;
    displayMode("light") -> displayBoardLight(Board) ;
    displayBoardAscii(Board).


displayBoardHeavy(Board) :-
    between(0, 41, X),
    (X == 0 -> writeln('┌────┬────┬────┬────┬────┬────┬────┐') ; true ),
    nth0(X, Board, Player), playerSymbol(Player, Symbol), format("│ ~w ", [Symbol]),
    ((X > 0, X < 41, (X+1) mod 7 =:= 0) -> writeln('│'), writeln('├────┼────┼────┼────┼────┼────┼────┤') ; true ),
    X == 41, writeln('│'), writeln('└────┴────┴────┴────┴────┴────┴────┘').

displayBoardLight(Board) :-
    between(0, 41, X),
    (X == 0 -> writeln('┌──────────────────────┐'), write('│ ') ; true ),
    nth0(X, Board, Player), playerSymbol(Player, Symbol), format("~w ", [Symbol]),
    ((X > 0, X < 41, (X+1) mod 7 =:= 0) -> writeln('│'), write('│ ') ; true ),
    X == 41, writeln('│'), writeln('└──────────────────────┘').

displayBoardAscii(Board) :-
    between(0, 41, X),
    (X == 0 -> writeln('+----------------------+'), write('| ') ; true ),
    nth0(X, Board, Player), playerSymbol(Player, Symbol), format("~w ", [Symbol]),
    ((X > 0, X < 41, (X+1) mod 7 =:= 0) -> writeln('|'), write('| ') ; true ),
    X == 41, writeln('|'), writeln('+----------------------+').

playerSymbol(Player, Symbol):-
    displayMode("ascii") -> (Player == 'X' -> Symbol = 'X ' ; Player == 'Y' -> Symbol = 'O ' ; Symbol = '. ') ;
    (Player == 'X' -> Symbol = '🟡' ; Player == 'Y' -> Symbol = '🔴' ; Symbol = '  ').

changePlayer('X','Y').
changePlayer('Y','X').

dropPiece(Board, Column, Move) :-
    between(0, 6, Row),
        Move is 35 + Column - 7*Row,
        nth0(Move, Board, Val),
        var(Val),
    !.

playMove(Board,Move,NewBoard,Player) :-
	copy_term(Board, NewBoard),
    nth0(Move, NewBoard,Player).

applyIt(Board, NewBoard) :-
    retract(board(Board)), % on oublie l'ancienne règle qui n'est plus vraie
    assert(board(NewBoard)).


isWinningMove(Board, Move) :-
    linecompleted(Board, Move);
    columncompleted(Board, Move);
    diagonalcompleted(Board, Move).


play(Player):- 
    board(Board), % instanciate the board from the knowledge base
    playerSymbol(Player, Symbol), write('New turn for: '), writeln(Symbol),

    % select the correct predicate (given in flags) to call for the current player
    (
        Player == 'X' -> getenv("p1", AiType);
        Player == 'Y' -> getenv("p2", AiType)
    ),

    (
        AiType == 'basic' -> basicAi(Board, Move, Player);
        AiType == 'minmax' -> minmaxIa(Board, Move, Player);
        AiType == 'random' -> randomIa(Board, Move, Player);
        AiType == 'user' -> userAi(Board, Move, Player);
        AiType == 'neural-network-sigmoid' -> neuralNetworkAiSigmoid(Board, Move, Player);
        AiType == 'neural-network-ReLu' -> neuralNetworkAiReLu(Board, Move, Player);
        writeln("Incorrect AI type"), fail
    ),

    playMove(Board,Move,NewBoard,Player), % Play the move and get the result in a new Board
	applyIt(Board, NewBoard), % Remove the old board from the KB and store the new one
    displayBoard(NewBoard), % print it
	( gameOver(NewBoard, Move, Details) ->
                    displayBoard(NewBoard),
                    format("Game over ! (~w)\n", [Details]), !
                ;
                    changePlayer(Player, NextPlayer),
                    play(NextPlayer)
            ).

init :-
    length(Board, 42),
    assert(board(Board)),
    play('X').



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%% IA %%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

