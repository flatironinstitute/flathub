FROM fpco/stack-build:lts-11.13
RUN apt-get update && \
    apt-get install -y libhdf5-dev && \
    rm -rf /var/lib/apt/lists/*
RUN useradd -m astrosims
USER astrosims

COPY --chown=astrosims stack.yaml astrosims.cabal /home/astrosims/astrosims/
WORKDIR /home/astrosims/astrosims
RUN stack install --system-ghc --extra-include-dirs=/usr/include/hdf5/serial --extra-lib-dirs=/usr/lib/x86_64-linux-gnu/hdf5/serial --only-dependencies

COPY --chown=astrosims js/package*.json js/jspm.config.js /home/astrosims/astrosims/js/
RUN cd js && npm install

COPY --chown=astrosims js/ /home/astrosims/astrosims/js/
RUN make -C js

COPY --chown=astrosims . /home/astrosims/astrosims
RUN stack install --system-ghc --extra-include-dirs=/usr/include/hdf5/serial --extra-lib-dirs=/usr/lib/x86_64-linux-gnu/hdf5/serial

EXPOSE 8092
ENTRYPOINT ["/home/astrosims/.local/bin/astrosims"]
CMD []
