FROM debian:bookworm-slim

RUN apt-get update && apt-get install -y --no-install-recommends gfortran make cmake && apt-get clean

COPY . /src/fastconv

WORKDIR /src/fastconv

RUN cmake . -DCMAKE_BUILD_TYPE=Release -B build && \
    cmake --build build && \
    cmake --install build && \
    rm -rf build

WORKDIR /var/work

ENTRYPOINT test_conv1d && test_conv2d
