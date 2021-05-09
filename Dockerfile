FROM rust:1.50

WORKDIR /app
RUN apt update && \
    apt install -y lsb-release wget software-properties-common && \
    wget https://apt.llvm.org/llvm.sh && \
    chmod +x llvm.sh && \
    ./llvm.sh 11 && \
    rm llvm.sh

ENV LLVM_SYS_110_PREFIX /usr/lib/llvm-11

COPY . .

RUN cargo install --path .

ENTRYPOINT ["/usr/local/cargo/bin/slip"]
CMD ["-"]
