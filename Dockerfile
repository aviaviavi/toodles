FROM haskell:8.4 as build-env

WORKDIR /app

RUN stack update

COPY toodles.cabal /app
COPY stack.yaml /app

RUN stack install --only-dependencies

COPY . /app

RUN stack install --ghc-options '-optl-static'

FROM alpine:latest

WORKDIR /app
COPY --from=build-env /app .
COPY --from=build-env /root/.local/bin/toodles /usr/local/bin/

VOLUME /repo

EXPOSE 9001

CMD ["toodles","-d","/repo/"]
