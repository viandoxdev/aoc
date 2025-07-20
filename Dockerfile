FROM ruby:3-bookworm AS build

RUN apt update
RUN apt-get install -y python3 python3-pip build-essential cmake git
RUN gem install github-linguist
WORKDIR /langs
RUN git clone https://github.com/viandoxdev/aoc .
RUN pip install -r requirements.txt --break-system-packages
RUN python3 langs.py

FROM scratch
COPY --from=build /langs/assets /
ENTRYPOINT ["/"]
