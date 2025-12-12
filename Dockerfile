FROM swipl:9.3.35

WORKDIR /app

COPY ./src src/
COPY ./Makefile ./
COPY ./src/bootstrap.example.pl src/bootstrap.pl

RUN apt-get update && apt-get install -y make && rm -rf /var/lib/apt/lists/*

CMD ["make"]
