#!/bin/bash
IA="random"
swipl -s ./src/game.pl -s ./src/gameover.pl -s ./src/bootstrap.pl -s ./src/ia/$IA.pl -g "bootstrap. halt."
