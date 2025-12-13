:- begin_tests(test_suite_name).

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

:- begin_tests(triplesRow).

test(get3AlignedInRow) :-
    Board = [
           _,_,_,_,_,_,_,
           _,_,_,_,_,_,'X',
           _  ,_  ,  _,  _,  _,  _,'X',
           'X',_  ,'X',_  ,'X', _ , _ ,
           _  ,'X',_  ,'X',_  ,_  ,'X',
           'X','X','Y',_,'X','X','X'
         ],
    get3AlignedInRow(Board, 39, 'X', C1R),
    assertion(C1R == 1).

:- end_tests(triplesRow).

:- begin_tests(triplesDiagonal).

test(get3AlignedInDiagonalDownRight) :-
    Board = [
               _,_,_,_,_,_,_,
               _,_,_,_,_,_,'X',
               _  ,_  ,  _,  _,  _,  _,'X',
               'X',_  ,'X',_  ,'X', _, _ ,
               _  ,'X',_  ,'X',_ ,_ ,'X',
               'X','X','Y',_,'X','X','X'
             ],
    get3AlignedInDiagonalDownRight(Board, 39, 'X', C1R),
    assertion(C1R == 1).

test(get3AlignedInDiagonalDownLeft) :-
    Board = [
               _,_,_,_,_,_,_,
               _,_,_,_,_,_,'X',
               _  ,_  ,  _,  _,  _,  _,'X',
               'X',_  ,'X',_  ,'X', _ , _ ,
               _  ,'X',_  ,'X',_  ,_  ,'X',
               'X','X','Y',_ ,'X','X', 'X'
             ],
    get3AlignedInDiagonalDownLeft(Board, 39, 'X', C1R),
    assertion(C1R == 0).

:- end_tests(triplesDiagonal).

:- begin_tests(triples).

test(countTriples1) :-
    List = [
      'X','X','Y','E','E','X','X'
    ],
    countTriples(List, 'X', Count),
    assertion(Count == 0).

test(countTriples2) :-
    List = [
      'X','X','Y','Y','X','X','X'
    ],
    countTriples(List, 'X', Count),
    assertion(Count == 0).

test(countTriples3) :-
    List = [
      'X','X','X','E','X','X','X'
    ],
    countTriples(List, 'X', Count),
    assertion(Count == 2).

test(countTriples4) :-
    List = [
      'E','X','X','X','E','X','X'
    ],
    countTriples(List, 'X', Count),
    assertion(Count == 1).

test(countTriples5) :-
    List = [
      'E','X','X','X','Y','X','X'
    ],
    countTriples(List, 'X', Count),
    assertion(Count == 1).

test(countTriples6) :-
    List = [
      'X','X','Y','Y','E','E','E'
    ],
    countTriples(List, 'X', Count),
    assertion(Count == 0).

test(countTriples7) :-
    List = [
      'X','X','Y','E', 'X','X','X'
    ],
    countTriples(List, 'X', Count),
    assertion(Count == 1).

test(countTriples8) :-
    List = [
      'E','E','E','E', 'X','X','X'
    ],
    countTriples(List, 'X', Count),
    assertion(Count == 1).

test(countTriples9) :-
    List = [
     'X','X','Y','E','X','X','X'
    ],
    countTriples(List, 'X', Count),
    assertion(Count == 1).

:- end_tests(triples).

:- begin_tests(tuples).

test(countTuples1) :-
    List = [
      'X','X','Y','E','E','X','X'
    ],
    countTuples(List, 'X', Count),
    assertion(Count == 1).

test(countTuples2) :-
    List = [
      'X','X','Y','Y','X','X','X'
    ],
    countTuples(List, 'X', Count),
    assertion(Count == 0).

test(countTuples3) :-
    List = [
      'X','X','E','E','Y','X','X'
    ],
    countTuples(List, 'X', Count),
    assertion(Count == 1).

test(countTuples4) :-
    List = [
      'E','X','X','Y','E','X','X'
    ],
    countTuples(List, 'X', Count),
    assertion(Count == 0).

test(countTuples5) :-
    List = [
      'E','X','X','Y','Y','X','X'
    ],
    countTuples(List, 'X', Count),
    assertion(Count == 0).

test(countTuples6) :-
    List = [
      'X','X','Y','Y','E','E','E'
    ],
    countTuples(List, 'X', Count),
    assertion(Count == 0).

test(countTuples7) :-
    List = [
      'X','X','Y','E', 'X','X','E'
    ],
    countTuples(List, 'X', Count),
    assertion(Count == 1).

test(countTuples8) :-
    List = [
      'E','E','E','E', 'X','Y','X'
    ],
    countTuples(List, 'X', Count),
    assertion(Count == 0).

test(countTuples9) :-
    List = [
     'X','X','E','E','X','X','Y'
    ],
    countTuples(List, 'X', Count),
    assertion(Count == 2).

:- end_tests(tuples).

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
      'X',_  ,_  ,_  ,'X',_  ,_  ,
      _  ,'X',_  ,'X',_  ,'X','X',
      'X','X','Y',_  ,'X','X',_
    ],
    getWeightPosition(Board, 39, 'X', Weight),
    assertion(Weight == 20).

:- end_tests(weight).

    Board = 42,
    assertion(Board == 42).

:- end_tests(test_suite_name).
