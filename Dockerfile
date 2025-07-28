# Use Ubuntu 22.04 as the base builder image
FROM ubuntu:22.04 AS build

# essentials
RUN apt-get update -y \
 && apt-get autoclean -y \
 && apt-get autoremove -y \
 && apt-get install -y apt-transport-https curl git software-properties-common build-essential rsync emacs-nox vim python3 zip gnupg wget openjdk-17-jdk maven \
      rebar3 \
      vim \
      nano \
      graphviz \
 && rm -rf /var/lib/apt/lists/*

# Modern Erlang and Elixir
RUN curl -1sLf 'https://dl.cloudsmith.io/public/rabbitmq/rabbitmq-erlang/gpg.key' | apt-key add - \
 && add-apt-repository 'deb https://dl.cloudsmith.io/public/rabbitmq/rabbitmq-erlang/deb/ubuntu jammy main' \
 && apt-get update -y \
 && apt-get install -y elixir erlang-dev erlang-eunit erlang-common-test erlang-dialyzer erlang-debugger erlang-parsetools erlang-runtime-tools erlang-os-mon erlang-ssl

# Packaging tools
RUN apt-get install -y make nsis tofrodos mandoc bsdmainutils

# Bazel installer
RUN curl -L --output /usr/local/bin/bazel https://github.com/bazelbuild/bazelisk/releases/download/v1.15.0/bazelisk-linux-amd64 \
 && chmod +x /usr/local/bin/bazel


WORKDIR /scribble-java

# Copy the wrapper
COPY mvnw .
COPY .mvn .mvn
RUN chmod +x ./mvnw


# Copy the parent POM + every module POM
COPY pom.xml .
COPY scribble-core/pom.xml       ./scribble-core/pom.xml
COPY scribble-parser/pom.xml     ./scribble-parser/pom.xml
COPY scribble-cli/pom.xml        ./scribble-cli/pom.xml
COPY scribble-gt/pom.xml         ./scribble-gt/pom.xml
COPY scribble-test/pom.xml       ./scribble-test/pom.xml
COPY scribble-ast/pom.xml        ./scribble-ast/pom.xml
COPY scribble-main/pom.xml       ./scribble-main/pom.xml


# Copy the full sources in and build
COPY scribble-core       ./scribble-core
COPY scribble-parser     ./scribble-parser
COPY scribble-cli        ./scribble-cli
COPY scribble-gt         ./scribble-gt
COPY scribble-test       ./scribble-test
COPY scribble-gt-demos   ./scribble-gt-demos
COPY scribble-ast        ./scribble-ast
COPY scribble-main       ./scribble-main


COPY header.txt .
COPY LICENSE.txt .

COPY mMST.sh .
RUN chmod +x ./mMST.sh

## Let the Maven wrapper autodetect Java from PATH rather than forcing JAVA_HOME
#ENV JAVA_HOME=/opt/java/openjdk
#ENV PATH=$JAVA_HOME/bin:$PATH
ENV MAVEN_SKIP_RC=true


RUN ./mvnw clean install \
      -Dstyle.color=never \
      -Dmaven.test.skip=true \
      -Dlicense.skip \
      -Dmaven.repo.local=/root/.m2/repository


## Copy demos before build
#COPY scribble-gt-demos   ./scribble-gt-demos
#
#RUN find /scribble-java/scribble-gt-demos/erlang/rabbitmq-server -type f -name '*.d' -delete
#
#WORKDIR /scribble-java/scribble-gt-demos/erlang/rabbitmq-server
#RUN make
