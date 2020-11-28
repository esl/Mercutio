FROM ubuntu:18.04 as common

RUN apt-get update
RUN apt-get -y install apt-utils openssl

FROM common as intermediate
##################################################################################
## Install erlang
##################################################################################
ARG otp_vsn=22.3.4

ADD https://packages.erlang-solutions.com/erlang-solutions_2.0_all.deb /tmp/
ADD https://packages.erlang-solutions.com/ubuntu/erlang_solutions.asc /tmp/

RUN apt-get -y install gnupg2
RUN dpkg -i /tmp/erlang-solutions_2.0_all.deb
RUN apt-key add /tmp/erlang_solutions.asc
RUN apt-get update
RUN apt-get -y install esl-erlang=1:${otp_vsn}-1

##################################################################################
## Install rebar & git
##################################################################################
ADD https://s3.amazonaws.com/rebar3/rebar3 /bin
RUN chmod a+x /bin/rebar3

RUN apt-get -y install git

##################################################################################
## Install packets to build native dependencies of escalus
##################################################################################
RUN apt-get -y install build-essential

##################################################################################
## Build mercutio
##################################################################################
WORKDIR /git
COPY . /git
RUN git clean -ffxd

RUN rebar3 tar

WORKDIR /app
RUN tar -xzf /git/_build/*/*/*/*.tar.gz

##################################################################################
## Create final image
##################################################################################
FROM common 

COPY --from=intermediate /app /app
ENV PATH="$PATH:/app/bin"

CMD ["/bin/bash", "-c", "mercutio foreground"]
