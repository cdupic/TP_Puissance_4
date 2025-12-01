#!/bin/zsh
{ cat ./src/script.pl ; echo -ne '\n\n' } | swipl ./src/game.pl
