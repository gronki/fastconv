FROM fedora

RUN dnf install -y gcc-gfortran gcc cmake && dnf clean all

COPY . /opt/fastconv

WORKDIR /opt/fastconv/build

RUN cmake .. -DCMAKE_BUILD_TYPE=Release && \
    cmake --build . --verbose && \
    cmake --install .

ENTRYPOINT test_conv1d && test_conv2d