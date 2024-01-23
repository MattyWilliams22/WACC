FROM openjdk:11

COPY . /wacc

WORKDIR /wacc

RUN apt-get update -y
RUN apt-get install make -y
RUN curl -fL https://github.com/Virtuslab/scala-cli/releases/latest/download/scala-cli-x86_64-pc-linux.gz | gzip -d > scala-cli
RUN chmod +x scala-cli
RUN mv scala-cli /usr/local/bin/scala-cli
