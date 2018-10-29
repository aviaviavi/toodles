FROM haskell:8.4

VOLUME /repo
WORKDIR /app

RUN stack update

COPY toodles.cabal /app
COPY stack.yaml /app

RUN stack install --only-dependencies

COPY . /app

RUN stack install

EXPOSE 9001

CMD ["toodles","-d","/repo/"]
