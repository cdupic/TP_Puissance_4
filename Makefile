p1=basic
p2=random

run:
	# Set env variables p1 and p2 according to the variables defined above:
	export p1=$(p1) p2=$(p2) && \
	swipl -s src/game.pl -s src/gameover.pl -s src/bootstrap.pl -s src/ia/basic.pl -s src/ia/minmax.pl -s src/ia/random.pl -g "init, halt."

test:
	swipl -s src/game.pl -s src/gameover.pl -s src/ia/minmax.pl -s src/test.pl -g "run_tests, halt."
