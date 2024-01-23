FROM openjdk:11

COPY . /wacc

WORKDIR /wacc

RUN apt-get update -y
RUN apt-get install make -y
RUN curl -fL "https://github.com/coursier/launchers/raw/master/cs-x86_64-pc-linux.gz" | gzip -d > cs
RUN chmod +x cs
RUN ./cs setup -y