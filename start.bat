set p1=user
set p2=minmax
swipl -s src/game.pl -s src/gameover.pl -s src/bootstrap.pl -s src/ia/basic.pl -s src/ia/minmax.pl -s src/ia/random.pl -s src/ia/user.pl -g "init, halt."