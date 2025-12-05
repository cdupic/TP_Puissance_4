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
    %MoveCol is Move mod 7,
    between(0, 5, R),
    TopRight is Move - 6*R,
    TopRightCol is TopRight mod 7,
    (TopRightCol == 6;  % si on arrive à la dernière colonne, exit
    TopRight < 7),  % ou on est sur la première ligne
    !.


getTopLeft(Move, TopLeft) :-
    %MoveCol is Move mod 7,
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

    

gameOver(Board, Move) :- 

    % check if a line as been completed
    linecompleted(Board, Move) -> format("line completed at move~w~n", [Move]) 
    ;
    % check if a column has been completed
    columncompleted(Board, Move) -> format("column completed at move~w~n", [Move])
    ;
    % check if a diagonal has been completed
    diagonalcompleted(Board, Move) -> format("diagonal completed at move~w~n", [Move])
    ;

    % else check whether board is full
    \+ (between(0, 41, X),
        nth0(X, Board, Val),
        var(Val)) -> write("Draw\n").



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
        AiType == 'basic-ai' -> basicAi(Board, Move, Player);
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


basicAi(Board, Move, Player) :-
    % play a winning move if there is one
    between(0, 6, Column),
        dropPiece(Board, Column, M),
        playMove(Board, M, HypotheticalBoard, Player),
        isWinningMove(HypotheticalBoard, M),
        Move = M,
    !
    ;

    % block a potential opponent winning move
    changePlayer(Player, Opponent),
    between(0, 6, Column),
        dropPiece(Board, Column, M),
        playMove(Board, M, HypotheticalBoard, Opponent),
        isWinningMove(HypotheticalBoard, M),
        Move = M,
    !
    ;

    % find a move that does not enable a direct win for the opponent
    % use a random permutation, as to not always play the same move
    random_permutation([0,1,2,3,4,5,6], MoveOrder),
    changePlayer(Player, Opponent),
    between(0, 6, N),
        nth0(N, MoveOrder, Column),
        dropPiece(Board, Column, M),
        playMove(Board, M, HypotheticalBoard, Player),

        % check that the move in this column does not allow the opponent to win (by playing in the same column)
        dropPiece(HypotheticalBoard, Column, OpponentM),
        playMove(HypotheticalBoard, OpponentM, HypotheticalBoard2, Opponent),
        \+ isWinningMove(HypotheticalBoard2, OpponentM),
        Move = M,
    !
    ;

    % default behaviour : play a random valid move
    repeat,
    Column is random(7), % choose random column
    dropPiece(Board, Column, Move),
    !.




