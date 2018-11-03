FROM haskell:8.4 as build-env

WORKDIR /app

RUN stack update

COPY toodles.cabal /app
COPY stack.yaml /app

RUN stack install --only-dependencies

COPY . /app

RUN stack install

FROM haskell:8.4

WORKDIR /app
COPY --from=build-env /app .
COPY --from=build-env /root/.local/bin/toodles /root/.local/bin/toodles

VOLUME /repo

EXPOSE 9001

CMD ["toodles","-d","/repo/"]
