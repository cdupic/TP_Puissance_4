:- begin_tests(diagonal).

test(getDiagonalDownRight) :-
    Board = [
      _,_,_,_,_,_,_,
      _,_,_,_,_,_,'X',
      _, 'X',_,_,_, 'X','X',
      'X',_, 'X',_, 'X',_,_,
      _, 'X',_, 'X',_,_, 'X',
      'X','X','Y','X','X','X','X'
    ],
    getDiagonalDownRight(Board, 39, Diagonal),
    assertion(Diagonal == ['E', 'X','X','X','X']).


test(getDiagonalDownLeft) :-
    Board = [
      _,_,_,_,_,_,_,
      _,_,_,_,_,_,'X',
      _, 'X',_,_,_, 'X','X',
      'X',_, 'X',_, 'X',_,_,
      _, 'X',_, 'X',_,_, 'X',
      'X','X','Y','X','X','X','X'
    ],
    getDiagonalDownLeft(Board, 39, Diagonal),
    assertion(Diagonal == ['E', 'E', 'X']).

:- end_tests(diagonal).


:- begin_tests(gameOver).

test(gameOver1) :-
    Board = [
      '.',_,_,_,_,_,_,
      _,_,_,_,_,_,'X',
      _,_,_,_,_,_,'X',
      'X',_, 'X',_, 'X',_,_,
      _, 'X',_, 'X',_,_, 'X',
      'X','X','Y',_, 'X',_, 'X'
    ],
    \+ gameOver(Board, 39, _).

test(gameOver2) :-
     Board = [
       _,_,_,_,_,_,_,
       _,_,_,_,_,_,'X',
       _,_,_,_,_,_,'X',
       'X',_, 'X',_, 'X',_,_,
       _, 'X',_, 'X',_,_, 'X',
       'X','X','Y','X','X','X','X'
     ],
     gameOver(Board, 39, _).


test(gameOver3) :-
     Board = [
       'X','X','X','Y','Y','X','Y',
       'Y','X','Y','X','Y','Y','X',
       'X','Y','X','Y','X','X','X',
       'X','Y','X','Y','X','Y','Y',
       'Y','X','Y','X','Y','X','X',
       'X','X','Y','X','X','Y','X'
     ],
     gameOver(Board, 39, _).

:- end_tests(gameOver).

:- begin_tests(weight).

test(getWeightPosition1) :-
    Board = [
      _,_,_,_,_,_,_,
      _,_,_,_,_,_,'X',
      _,_,_,_,_,_,'X',
      'X',_, 'X',_, 'X',_,_,
      _, 'X',_, 'X',_,_, 'X',
      'X','X','Y',_, 'X','X','X'
    ],
    getWeightPosition(Board, 39, 'X', Weight),
    assertion(Weight == 200).

test(getWeightPosition2) :-
    Board = [
      _,_,_,_,_,_,_,
      _,_,_,_,_,_,'X',
      _,_,_,_,_,_,'X',
      'X',_,_,_, 'X',_,_,
      _, 'X',_, 'X',_, 'X','X',
      'X','X','Y',_, 'X','X',_
    ],
    getWeightPosition(Board, 39, 'X', Weight),
    assertion(Weight == 40).

:- end_tests(weight).



% swipl -s src/gameover.pl -s src/ia/minmax.pl -s src/test.pl -g "run_tests, halt."
