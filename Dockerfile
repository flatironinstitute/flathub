FROM fpco/stack-build:lts-11.6
RUN apt-get update && \
    apt-get install -y libhdf5-dev && \
    rm -rf /var/lib/apt/lists/*
RUN useradd -m astrosims
USER astrosims
COPY --chown=astrosims . /home/astrosims/astrosims
WORKDIR /home/astrosims/astrosims
RUN stack install --system-ghc --extra-include-dirs=/usr/include/hdf5/serial --extra-lib-dirs=/usr/lib/x86_64-linux-gnu/hdf5/serial
RUN cd js && npm install && npm run build
EXPOSE 8092
ENTRYPOINT ["/home/astrosims/.local/bin/astrosims"]
CMD []
