FROM debian:stretch

RUN apt-get update
RUN apt-get -y install curl
RUN curl -sSL https://get.haskellstack.org/ | sh
ADD . /chess
WORKDIR /chess
RUN stack build

#APP stack run