FROM debian:stretch

RUN apt-get update && apt-get install -y curl wget netbase

RUN curl -L https://scarf.sh/install | bash

ENV PATH $PATH:~/.scarf/bin

RUN ~/.scarf/bin/scarf install toodles

CMD ["/root/.scarf/bin/toodles","-d","/repo/"]
