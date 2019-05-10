FROM haskell:8.4.3 as build-env

WORKDIR /toodles-app

RUN stack update

COPY package.yaml /toodles-app/
COPY stack.yaml /toodles-app/
COPY app/ /toodles-app/app
COPY src/ /toodles-app/src
COPY test/ /toodles-app/test
COPY web/ /toodles-app/web
COPY README.md /toodles-app/
COPY toodles-license-public-key.pem /toodles-app/
COPY verify.py /toodles-app/

RUN stack install --only-dependencies

RUN stack install

FROM debian:stretch

WORKDIR /toodles-app
COPY --from=build-env /toodles-app .
COPY --from=build-env /root/.local/bin/toodles /usr/local/bin/

VOLUME /repo

EXPOSE 9001

# Due to issues described in https://github.com/aviaviavi/toodles/issues/54, we
# have to install stack to make the binary from the previous step work in our
# container
RUN apt-get update
RUN apt-get install -y wget build-essential
RUN wget -qO- https://get.haskellstack.org/ | sh

CMD ["toodles","-d","/repo/"]
