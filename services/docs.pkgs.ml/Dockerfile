ENV DEBIAN_FRONTEND=noninteractive
WORKDIR /workspace

RUN apt-get update && apt-get install -y \
    build-essential \
    ca-certificates \
    curl \
    git \
    python3 \
    tar \
    libssl-dev \
    uuid-dev \
    libzstd-dev \
    && rm -rf /var/lib/apt/lists/*

COPY services/docs.pkgs.ml/src/container_server.py /usr/local/bin/docs-pipeline-server

RUN chmod +x /usr/local/bin/docs-pipeline-server

EXPOSE 8080

ENTRYPOINT ["python3", "/usr/local/bin/docs-pipeline-server"]
