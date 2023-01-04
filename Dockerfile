FROM fedora:latest

RUN dnf install -y gcc-gfortran gcc cmake

COPY . /opt/fastconv

WORKDIR /opt/fastconv/build

RUN cmake .. -DCMAKE_BUILD_TYPE=Release && \
    cmake --build . --verbose && \
    cmake --install .

RUN dnf clean all

ENTRYPOINT test_conv1d && test_conv2d