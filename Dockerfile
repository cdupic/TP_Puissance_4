FROM swipl:9.3.35

WORKDIR /app

COPY ./src src/
COPY ./start.sh ./

RUN chmod +x start.sh
CMD ["/app/start.sh"]
