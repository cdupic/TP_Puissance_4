
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




