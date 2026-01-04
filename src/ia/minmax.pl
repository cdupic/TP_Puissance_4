minmaxIa(Board, FinalMove, Mark):-
   prepare_board(Board, BoardNewFormat),
   minimax(0,BoardNewFormat,Mark,Col,_, Mark),
   dropPiece2(BoardNewFormat,Col, Move),
   FinalMove is Move - 1,
   nl, nl, write('MinMax places '), write(Mark), write(' in Col '),
         write(Col), write('.').



% Convertit le board (avec _) vers le format (avec 'e')
prepare_board([], []).
prepare_board([H|T], ['e'|OutT]) :-
   var(H), !,             % Si c'est une variable non instanciée
   prepare_board(T, OutT).
prepare_board([H|T], [H|OutT]) :-
   prepare_board(T, OutT).


depthMax(4).
free_cell('e').
next_player(1,2).      %%% determines the next player after the given player
next_player(2,1).


board_dims(7, 6).   % Largeur = 7, Hauteur = 6

inverse_mark('X','Y').  %%% determines the opposite of the given mark
inverse_mark('Y','X').

player_mark(1,'X').    %%% the mark for the given player
player_mark(2,'Y').


opponent_mark(1, 'Y').  %%% the inverse mark of the given player
opponent_mark(2, 'X').

game_over(Player,Board) :- opponent_mark(Player, Mark), win(Board, Mark), !.
game_over(_,Board) :- isFull(Board).


isFull([]).
isFull([H|T]) :- H \== 'e', isFull(T).

move(Board1,Square,Mark,Board2) :- set_item(Board1,Square,Mark,Board2).


set_item(List1,Position,Val,List2) :-
nth1(Position,List1,_,List3), nth1(Position,List2,Val,List3), !.


utility(Board,_Mark,Utility, MarkIa) :-
getWeightBoard(Board,MarkIa, WeightAI),
inverse_mark(MarkIa,MarkHuman),
getWeightBoard(Board,MarkHuman, WeightH),
Utility is WeightAI - WeightH,!.

possible_moves(Board,List)
:- not(win(Board,'X')), %%% if either player already won,
                 %%% then there are no available moves
  not(win(Board,'Y')),
  findall(Col,(between(1,7,Col), dropPiece2(Board,Col, _Move)), List). % Fail if List should be empty %-----------------------------------------test list not empty ???????????????????


% Drop dans un tableau 7*6
dropPiece2(Board, Column, Move) :-
board_dims(W, H),
H_rev is H - 1,
between(0, H_rev, Row_rev), % Itère les lignes de bas en haut (0=dernière ligne)
Row is H_rev - Row_rev,
Index is Row * W + (Column - 1),
nth0(Index, Board, 'e'),    % La case est-elle vide ?
Move is Index + 1,          % Renvoie la position (base 1)
!.


matricePoids([
 3, 4,  5,  7,  5, 4, 3,   % Ligne 1 (Haut)
 4, 6,  8, 10,  8, 6, 4,   % Ligne 2
 5, 8, 11, 13, 11, 8, 5,   % Ligne 3
 5, 8, 11, 13, 11, 8, 5,   % Ligne 4
 4, 6,  8, 10,  8, 6, 4,   % Ligne 5
 3, 4,  5,  7,  5, 4, 3    % Ligne 6 (Bas)
]).


getWeightBoard(Board,Mark, Weight) :-
win(Board, Mark) ->
           Weight = 100000;
matricePoids(MatricePoids),
get2Aligned(Board, Mark, Nbr2),
get3Aligned(Board, Mark, Nbr3),

findall(W,
(
between(0,41,Index),
nth0(Index, Board, Mark),
nth0(Index, MatricePoids, W)
),
Ws),
sum_list(Ws,WeightStatic),

WeightAligne is Nbr2*10 +Nbr3*100,
%writeln(Weight2),
Weight is WeightAligne+WeightStatic.

minimax(_Depth,Board,_Mark,ColToPlay,_Utility, _MarkIa) :-
 \+ memberchk('X', Board), \+ memberchk('Y', Board), !, % Si le plateau est vide
 ColToPlay is 4. % Joue au centre (colonne 4)


minimax(Depth,Board,Mark,ColToPlay,Utility, MarkIa) :-
not(depthMax(Depth)),
Depth2 is Depth+1,
possible_moves(Board,List),    %%% get the list of possible moves
   best(Depth2,Board,Mark,List,ColToPlay,Utility, MarkIa).
               %%% recursively determine the best available move


minimax(Depth,Board,Mark,_ColToPlay,Utility, MarkIa) :-
( win(Board,'X'); win(Board,'Y');depthMax(Depth);game_over(_,Board)),
utility(Board, Mark, Utility, MarkIa).  % ← LastMove est valide


best(Depth,Board,Mark,[ColToPlay],ColToPlay,Utility, MarkIa) :-
dropPiece2(Board, ColToPlay, Square),
move(Board,Square,Mark,Board2),
inverse_mark(Mark,Mark2),
minimax(Depth,Board2,Mark2,_,Utility, MarkIa),
%output_value(Depth,ColToPlay,Utility),
!.


best(Depth,Board,Mark,[Col1|Other_Moves],ColToPlay,Utility, MarkIa)
:- dropPiece2(Board, Col1, Square),
move(Board,Square,Mark,Board2),   %%% apply the first move (in the list)
 inverse_mark(Mark,Mark2),
 minimax(Depth,Board2,Mark2,_,Utility1, MarkIa),
%output_value(Depth,Col1,Utility1),
 best(Depth,Board,Mark,Other_Moves,ColToPlay2,Utility2, MarkIa),
 better(Depth,Mark,Col1,Utility1,ColToPlay2,Utility2,ColToPlay,Utility, MarkIa).


better(_Depth,Mark,Col1,Utility1,_ColToPlay2,Utility2,Col1,Utility1,MarkIa)
:- Mark == MarkIa,          %%% if the player is maximizing
  Utility1 > Utility2, !.       %%% then greater is better.


better(_Depth,Mark,Col1,Utility1,_ColToPlay2,Utility2,Col1,Utility1,MarkIa)
:- Mark \==MarkIa,          %%% if the player is minimizing,
  Utility1 < Utility2, !.       %%% then lesser is better.


better(_Depth,Mark,Col1,Utility1,ColToPlay2,Utility2,ColToPlay,Utility, _MarkIa)
:- Utility1 == Utility2,     %%% if moves have equal utility,
  random_between(1,10,R),       %%% then pick one of them at random
  better2(_,R,Mark,Col1,Utility1,ColToPlay2,Utility2,ColToPlay,Utility), !.


better(_Depth,_Mark,_Col1,_Utility1,ColToPlay2,Utility2,ColToPlay2,Utility2, _MarkIa).
                       %%% otherwise, second move is better


better2(_,R,_Mark,Col1,Utility1,_ColToPlay2,_Utility2,Col1,Utility1) :- R < 6, !.
better2(_,_R,_Mark,_Col1,_Utility1,ColToPlay2,Utility2,ColToPlay2,Utility2).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Accès à une cellule
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% X = colonne (0..6)
% Y = ligne    (0..5)


cell(Board, W, X, Y, Val) :-
Index is Y*W + X,
nth0(Index, Board, Val).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Lignes
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


row(Board, W, Y, Row) :-
W1 is W - 1,
findall(V,
    ( between(0, W1, X),
      cell(Board, W, X, Y, V)
    ),
    Row).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Colonnes
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


column(Board, W, H, X, Col) :-
H1 is H - 1,
findall(V,
    ( between(0, H1, Y),
      cell(Board, W, X, Y, V)
    ),
    Col).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Diagonales (down-right)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


diag_dr(Board, W, H, X, Y, Diag) :-
Max is min(W, H) - 1,
findall(V,
    ( between(0, Max, I),
      X1 is X + I,
      Y1 is Y + I,
      X1 < W,
      Y1 < H,
      cell(Board, W, X1, Y1, V)
    ),
    Diag).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Diagonales (down-left)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


diag_dl(Board, W, H, X, Y, Diag) :-
Max is min(W, H) - 1,
findall(V,
    ( between(0, Max, I),
      X1 is X - I,
      Y1 is Y + I,
      X1 >= 0,
      Y1 < H,
      cell(Board, W, X1, Y1, V)
    ),
    Diag).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Comptage de 2 et 3 alignés dans une liste
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Chevauchement autorisé : [x,x,x] => 2


count_2aligned(List, Mark, Count) :-
   count_2aligned_(List, Mark, 0, Count).


% cas : _ M M
count_2aligned_([Cell,Mark,Mark|Rest], Mark, Acc, Count) :-
   free_cell(Cell),
   Acc1 is Acc + 1,
   count_2aligned_(Rest, Mark, Acc1, Count).


% cas : M M _
count_2aligned_([Mark,Mark,Cell|Rest], Mark, Acc, Count) :-
   free_cell(Cell),
   Acc1 is Acc + 1,
   count_2aligned_(Rest, Mark, Acc1, Count).


% sinon on décale
count_2aligned_([_|Rest], Mark, Acc, Count) :-
   count_2aligned_(Rest, Mark, Acc, Count).


count_2aligned_([], _, Count, Count).
count_2aligned_([_], _, Count, Count).


count_2aligned_for(Mark, Line, Count) :-
   count_2aligned(Line, Mark, Count).

countTriples(List, Player, Count) :-
   countTriplesAux(List, Player, 0, Count).


% cas : _ P P P
countTriplesAux([Cell,Player,Player,Player|Rest], Player, Acc, Count) :-
   free_cell(Cell),
   Acc1 is Acc + 1,
   countTriplesAux(Rest, Player, Acc1, Count).


% cas : P P P _
countTriplesAux([Player,Player,Player,Cell|Rest], Player, Acc, Count) :-
   free_cell(Cell),
   Acc1 is Acc + 1,
   countTriplesAux(Rest, Player, Acc1, Count).


% sinon on décale la fenêtre
countTriplesAux([_|Rest], Player, Acc, Count) :-
   countTriplesAux(Rest, Player, Acc, Count).


countTriplesAux([], _, Count, Count).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Calcul GLOBAL : lignes + colonnes + diagonales
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


get2Aligned(Board, Mark, Total) :-
board_dims(W, H),
H1 is H - 1,
% Lignes
findall(Row,
    ( between(0, H1, Y),
      row(Board, W, Y, Row)
    ),
    Rows),
W1 is W - 1,
% Colonnes
findall(Col,
    ( between(0, W1, X),
      column(Board, W, H, X, Col)
    ),
    Cols),

% Diagonales ↘ et ↙
findall(D,
    (
        between(0, W1, X), diag_dr(Board, W, H, X, 0, D)
    ;   between(1, H1, Y), diag_dr(Board, W, H, 0, Y, D)
    ;   between(0, W1, X), diag_dl(Board, W, H, X, 0, D)
    ;   between(1, H1, Y), diag_dl(Board, W, H, W1, Y, D)
    ),
    Diags),

append([Rows, Cols, Diags], AllLines),
maplist(count_2aligned_for(Mark), AllLines, Counts),
sum_list(Counts, Total).


get3Aligned(Board, Mark, Total) :-
board_dims(W,H),
H1 is H - 1,
W1 is W - 1,


% Lignes
findall(Row, (between(0,H1,Y), row(Board,W,Y,Row)), Rows),
% Colonnes
findall(Col, (between(0,W1,X), column(Board,W,H,X,Col)), Cols),
% Diagonales ↘ et ↙
findall(D,
    (
        between(0,W1,X), diag_dr(Board,W,H,X,0,D)
    ;   between(1,H1,Y), diag_dr(Board,W,H,0,Y,D)
    ;   between(0,W1,X), diag_dl(Board,W,H,X,0,D)
    ;   between(1,H1,Y), diag_dl(Board,W,H,W1,Y,D)
    ),
    Diags),

append([Rows,Cols,Diags], AllLines),
maplist(countTriplesFor(Mark), AllLines, Counts),
sum_list(Counts, Total).


countTriplesFor(Mark, Line, Count) :-
countTriples(Line, Mark, Count).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                 WIN                  %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
win(Board, Mark) :-
  board_dims(W,H),
  W1 is W-1,
  H1 is H-1,
  between(0, W1, X),
  between(0, H1, Y),
  cell(Board, W, X, Y, Mark),
  (
      win_from(Board, Mark, X, Y, 1, 0)  % →
  ;   win_from(Board, Mark, X, Y, 0, 1)  % ↓
  ;   win_from(Board, Mark, X, Y, 1, 1)  % ↘
  ;   win_from(Board, Mark, X, Y, -1, 1) % ↙
  ),
  !.
%Diag right topbottom
win_from(Board, Mark, X, Y, -1, 1) :-
board_dims(W, H),
  forall(between(0,3,I),
      (X1 is X-I,
       Y1 is Y+I,
        X1 >= 0, X1 < W,
        Y1 >= 0, Y1 < H,
        cell(Board, W, X1, Y1, Mark))).

%Diag left topbottom
win_from(Board, Mark, X, Y, 1, 1) :-
board_dims(W, H),
  forall(between(0,3,I),
      (X1 is X+I,
       Y1 is Y+I,
       X1 >= 0, X1 < W,
     Y1 >= 0, Y1 < H,
     cell(Board,W,X1,Y1,Mark))).

%vertical
win_from(Board, Mark, X, Y, 0, 1) :-
board_dims(W, H),
  forall(between(0,3,I),
      (Y1 is Y+I,
      Y1 >= 0, Y1 < H,
      cell(Board,W,X,Y1,Mark))).

%Horizontal
win_from(Board, Mark, X, Y, 1, 0) :-
board_dims(W, _H),
  forall(between(0,3,I),
      (X1 is X+I,
      X1 >= 0, X1 < W,
      cell(Board,W,X1,Y,Mark))).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
