FROM alpine:3.12 as builder

# Dev dependencies
RUN apk --no-cache add --repository http://dl-cdn.alpinelinux.org/alpine/edge/community \
        ca-certificates git ghc=8.8.3-r0 upx curl musl-dev gmp-dev zlib-dev zlib-static glib-static pcre-dev libx11-dev libxrandr-dev

# Stack
RUN curl -L https://github.com/commercialhaskell/stack/releases/download/v2.3.1/stack-2.3.1-linux-x86_64-static.tar.gz | tar -xz ; \
    cp stack-2.3.1-linux-x86_64-static/stack /usr/bin/

# Build dependencies
COPY stack.yaml /mnt
COPY *.cabal /mnt
WORKDIR /mnt
RUN rm -rf ~/.stack &&  \
    stack config set system-ghc --global true && \
    stack setup && \
    stack install --split-objs --ghc-options="-O2 -fPIC -fllvm" --only-dependencies

# Load project
COPY . /mnt
RUN rm ntfd.cabal

# Uncomment build options required for static linking and compile binary
RUN mv package.yaml p.yaml; sed 's/## //' p.yaml > package.yaml
RUN stack install --split-objs

# Load compiled binary in a production ready container
FROM alpine:3.12 as runner

WORKDIR /root
COPY --from=builder /root/.local/bin/ntfd .
RUN chmod +x ./ntfd

CMD ["./ntfd"]
