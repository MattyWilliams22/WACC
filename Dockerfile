FROM openjdk:11

COPY . /wacc

WORKDIR /wacc

RUN apt-get update -y
RUN apt-get install make -y
RUN curl -fL https://github.com/Virtuslab/scala-cli/releases/latest/download/scala-cli-x86_64-pc-linux.gz | gzip -d > scala-cli
RUN chmod +x scala-cli
RUN mv scala-cli /usr/local/bin/scala-cli
RUN apt install -y python3
RUN apt install -y gcc gcc-arm-linux-gnueabi
RUN apt install -y libc6-armel-cross libc6-dev-armel-cross binutils-arm-linux-gnueabi libncurses5-dev build-essential bison flex libssl-dev bc
RUN apt install -y qemu-user