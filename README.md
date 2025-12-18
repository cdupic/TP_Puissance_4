# Connect 4 Game with various AIs implemented in SWI Prolog

## Play locally

### Setup the environment

First copy `src/bootstrap.example.pl` to `src/bootstrap.pl` and edit it to configure the game.
```bash
cp src/bootstrap.example.pl src/bootstrap.pl
```

By editing bootstrap.pl, you can choose an appropriate display mode for your terminal.

### Starting a game

To start a game, you can run start.bat or start.sh depending on your OS.

To select which AI you want to use, edit the first 2 lines of the start script :

```bash
set p1=basic
set p2=user
```

This creates a game in which the basic AI will play against the user.

valid options are :
 - basic
 - minmax
 - random
 - user
 - neural-network-sigmoid *
 - neural-network-ReLu * 

Please note that the options marked with * are only available on windows, due to the neural network binaries not being compiled for linux yet.

## Run on docker

First, start the docker container:
```bash
docker compose -f local-compose.yml up -d aliapr-prolog
```

Then, open an interactive shell in the container:
```bash
docker exec -it aliapr-prolog bash
```

Finally, run commands as usual.
```bash
make
```

Files are mounted in the container, so changes in the host are immediately reflected in the container.

## Run the web server

Start the webserver docker container:
```bash
docker compose -f local-compose.yml up -d aliapr-server
```

Then, open a browser and go to http://localhost:3000/
