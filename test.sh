#!/bin/bash
swipl -s src/gameover.pl -s src/ia/minmax.pl -s src/test.pl -g "run_tests, halt."
