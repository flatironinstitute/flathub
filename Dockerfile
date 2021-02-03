FROM fpco/stack-build-small:lts-16.31
RUN apt-get update && \
    apt-get install -y libhdf5-dev libbz2-dev pkg-config npm && \
    rm -rf /var/lib/apt/lists/*
RUN echo /opt/ghc/*/lib/ghc-*/rts > /etc/ld.so.conf.d/ghc.conf && \
    ldconfig
RUN useradd -u 999 -m astrosims
USER astrosims

COPY --chown=astrosims stack.yaml *.cabal Setup.hs COPYING /home/astrosims/astrosims/
WORKDIR /home/astrosims/astrosims
RUN stack build --dependencies-only --extra-include-dirs=/usr/include/hdf5/serial --extra-lib-dirs=/usr/lib/x86_64-linux-gnu/hdf5/serial
COPY --chown=astrosims src ./src
RUN stack install && rm -rf .stack-work
COPY --chown=astrosims web ./web
RUN make -C web
COPY --chown=astrosims html ./html
COPY --chown=astrosims config catalogs.yml ./

EXPOSE 8092
ENTRYPOINT ["/home/astrosims/.local/bin/astrosims"]
CMD []
ENV LD_LIBRARY_PATH=/home/stackage/.stack/programs/x86_64-linux/ghc-8.8.4/lib/ghc-8.8.4/rts
