FROM ubuntu

ENV TOODLES_DATA_DIR /app

VOLUME /repo

COPY web /app
copy toodles /app

EXPOSE 9001

CMD ["toodles","-d","/repo/"]
