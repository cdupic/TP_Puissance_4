set p1=basic
set p2=random
swipl -s src/game.pl -s src/gameover.pl -s src/bootstrap.pl -s src/ia/basic.pl -s src/ia/minmax.pl -s src/ia/random.pl -g "init, halt."