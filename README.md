# Connect 4 Game with various AIs implemented in SWI Prolog

## Run

First copy `src/bootstrap.example.pl` to `src/bootstrap.pl` and edit it to configure the game.
```bash
cp src/bootstrap.example.pl src/bootstrap.pl
```

Then, run `run.sh` on Unix or `run.bat` on Windows.

As of now, the IA selection is done by editing environment variables in the run file. The variables must match the filename of the chosen AI.

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
