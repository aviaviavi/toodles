FROM haskell:8.4 as builder

VOLUME /repo

WORKDIR /app

RUN stack update

COPY toodles.cabal /app
COPY stack.yaml /app

RUN stack install --only-dependencies

COPY . /app

RUN stack install

# TODO - use a multi stage build to reduce the final image size
FROM debian:stretch
WORKDIR /
VOLUME /repo
RUN apt-get update && apt-get install libgmp10
COPY --from=builder /root/.local/bin/toodles .
#COPY --from=builder /app/web /repo/
CMD ["/toodles","-d","/repo/"]
EXPOSE 9001
