FROM haskell:8.4

VOLUME /repo
WORKDIR /app
ADD . /app

RUN stack setup
RUN stack build --test --copy-bins

EXPOSE 9001

CMD ["toodles -d /repo"]
