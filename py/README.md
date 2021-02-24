# Flathub Query Module

A query module created to access any of the simulation catalogs hosted on flathub.flatironinstitute.org.

## Getting Started

[![Binder](https://mybinder.org/badge.svg)](https://mybinder.org/v2/gh/flatironinstitute/flathub/binder)

### Prerequisites

To install the module, you just need Python and numpy.

### Installing

To install, just clone this repository and run setup.py:

```
git clone git://github.com/flatironinstitute/flathub
cd flathub/py
python setup.py install
```

Once you have successfully installed the module, you can use it as:

```
import flathub.client
```

## Running the Module

To start off, create a Catalog object based off the simulation you want to query. From there, you can create a Query object and can query based off fields, sample, seed, and sort).

```
gaea = flathub.client.Catalog('gaea')
q = gaea.query(fields = ['PPos_x', 'PPos_y', 'z'], Mvir = (4700, 4900), sample = 0.01, seed = 0)
```

A dict of available fields is available on the Catalog:

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
