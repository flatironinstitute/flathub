FROM fpco/stack-build-small:lts-16.23
RUN apt-get update && \
    apt-get install -y libhdf5-dev libbz2-dev pkg-config npm && \
    rm -rf /var/lib/apt/lists/*
RUN echo /opt/ghc/*/lib/ghc-*/rts > /etc/ld.so.conf.d/ghc.conf && \
    ldconfig
RUN useradd -u 999 -m astrosims
USER astrosims

COPY --chown=astrosims . /home/astrosims/astrosims
WORKDIR /home/astrosims/astrosims
RUN stack install --system-ghc --extra-include-dirs=/usr/include/hdf5/serial --extra-lib-dirs=/usr/lib/x86_64-linux-gnu/hdf5/serial && \
    rm -rf .stack-work
RUN make -C web

EXPOSE 8092
ENTRYPOINT ["/home/astrosims/.local/bin/astrosims"]
CMD []
ENV LD_LIBRARY_PATH=/home/stackage/.stack/programs/x86_64-linux/ghc-8.8.4/lib/ghc-8.8.4/rts
