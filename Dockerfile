FROM fpco/stack-build:lts-11.22
RUN apt-get update && \
    apt-get install -y libhdf5-dev && \
    rm -rf /var/lib/apt/lists/*
RUN useradd -u 999 -m astrosims
USER astrosims

COPY --chown=astrosims stack.yaml astrosims.cabal /home/astrosims/astrosims/
WORKDIR /home/astrosims/astrosims
RUN stack install --system-ghc --extra-include-dirs=/usr/include/hdf5/serial --extra-lib-dirs=/usr/lib/x86_64-linux-gnu/hdf5/serial --only-dependencies

COPY --chown=astrosims web/package*.json web/jspm.config.js /home/astrosims/astrosims/web/
RUN cd web && npm install

COPY --chown=astrosims web/ /home/astrosims/astrosims/web/
RUN make -C web

COPY --chown=astrosims . /home/astrosims/astrosims
RUN stack install --system-ghc --extra-include-dirs=/usr/include/hdf5/serial --extra-lib-dirs=/usr/lib/x86_64-linux-gnu/hdf5/serial

EXPOSE 8092
ENTRYPOINT ["/home/astrosims/.local/bin/astrosims"]
CMD []
