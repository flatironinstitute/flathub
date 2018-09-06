# Astrosims Query Module

A query module created to access any of the simulation catalogs hosted on astrosims.flatironinstitute.org.

## Getting Started

### Prerequisites

To install the module, you just need Python and numpy.

### Installing

To install, just clone this repository and run setup.py:

```
git clone git://github.com/flatironinstitute/astrosims-reproto
cd astrosims-reproto/py
python setup.py install
```

Once you have successfully installed the module, you can use it as:

```
import fi_astrosims.client
```

## Running the Module

To start off, create a Simulation object based off the simulation you want to query. From there, you can create a Query object and can query based off fields, sample, seed, and sort).

```
gaea = fi_astrosims.client.Simulation('gaea')
q = gaea.query(fields = ['RA', 'Dec', 'z'], Mvir = (4700, 4900), sample = 0.01, seed = 0)
```

A dict of available fields is available on the Simulation:

```
gaea.fields
```

If you want to view some basic statistics about your Query, you can use functions like count(), avg(), or aggs():

```
q.count()
q.aggs('z')
```

Finally, you can retrieve the results your query using the numpy() function, which will download the query into a numpy array:

```
x = q.numpy()
```

## License

This project is licensed under the Apache 2 License - see the [COPYING](../COPYING) file for details.
