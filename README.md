# Simple tabular data query interface

This is a web-based interface for querying large numeric tabular datasets, designed primarily for halo object catalogs from astrophysical simulations.
It is built on Elasticsearch to allow efficient filtering and aggregation of datasets with hundreds of millions of objects.

## Installation

There is a [docker-compose](docker-compose.yml) definition for installing everything you need.  To use it:

- Edit the [catalog definitions](catalogs.yml) to define the object catalog(s) and fields.  There are a number of examples there already.
- Change `ES_JAVA_OPTS` and `scale` in docker-compose.yml as appropriate for your environment and needs (currently requires 100+GB memory)
   - You can also setup your own elasticsearch cluster on multiple machines, in which case you only need the astrosims service
   - Update [config](config) to point to your elasticsearch cluster if it's not the default
- Run `docker-compose up -d` to bring up everything (the astrosims service will exit with an error after building successfully -- this is expected)
- Run `docker-compose run astrosims -s *CATALOG*` for each defined catalog to create the indices
- Run `docker-compose run astrosims -i *CATALOG* *FILE* ...` to ingest data into each catalog
- Run `docker-compose up -d` to bring up the astrosims webserver
