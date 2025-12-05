#!/bin/bash
IA="random"
swipl -s ./src/game.pl -s ./src/gameover.pl -s ./src/main.pl -s ./src/ia/$IA.pl -g "main. halt."
