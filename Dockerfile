FROM fpco/stack-build-small:lts-21.25 AS base
RUN useradd -u 999 -m flathub
COPY --chown=flathub stack.yaml *.cabal Setup.hs COPYING /home/flathub/flathub/
WORKDIR /home/flathub/flathub


FROM base AS build
RUN apt-get update && \
    apt-get upgrade -y && \
    apt-get install -y libhdf5-dev libbz2-dev pkg-config && \
    rm -rf /var/lib/apt/lists/*

USER flathub
RUN stack build --dependencies-only --extra-include-dirs=/usr/include/hdf5/serial --extra-lib-dirs=/usr/lib/x86_64-linux-gnu/hdf5/serial
COPY --chown=flathub src ./src
RUN stack install


FROM base
ADD https://deb.nodesource.com/gpgkey/nodesource.gpg.key /tmp/
RUN apt-key add /tmp/nodesource.gpg.key && \
    echo deb https://deb.nodesource.com/node_18.x jammy main > /etc/apt/sources.list.d/nodesource.list && \
    apt-get update && \
    apt-get upgrade -y && \
    apt-get install -y libhdf5-103 bzip2 nodejs vim curl && \
    rm -rf /var/lib/apt/lists/*

EXPOSE 8092
ENTRYPOINT ["/home/flathub/.local/bin/flathub"]
CMD []
ENV LD_LIBRARY_PATH=/home/stackage/.stack/programs/x86_64-linux/ghc-9.2.5/lib/ghc-9.2.5/rts
USER flathub

COPY --chown=flathub web ./web
RUN make -C web && rm -rf web/node_modules
COPY --chown=flathub flatfront ./flatfront
RUN cd flatfront && npm install && npm run build
COPY --from=build /home/flathub/.local/bin/flathub /home/flathub/.local/bin/flathub
COPY html ./html
COPY config ./config
COPY catalogs ./catalogs
