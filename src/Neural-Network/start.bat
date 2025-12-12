set p1=neural-network-sigmoid
set p2=user
swipl  ^
    -s .\..\game.pl ^
    -s .\..\gameover.pl ^
    -s .\..\bootstrap.pl ^
    -s .\..\ia\basic.pl ^
    -s .\..\ia\minmax.pl ^
    -s .\..\ia\neural-network-sigmoid.pl ^
    -s .\..\ia\neural-network-ReLu.pl ^
    -s .\..\ia\user.pl ^
    -s .\..\ia\random.pl ^
    -g "init, halt."