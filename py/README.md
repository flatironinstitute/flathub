# Flathub Query Module

A query module created to access any of the simulation catalogs hosted on flathub.flatironinstitute.org.

## Getting Started

[![Binder](https://mybinder.org/badge.svg)](https://mybinder.org/v2/gh/flatironinstitute/flathub/binder)

### Prerequisites

To install the module, you just need Python and numpy.

### Installing

To install, just clone this repository and run setup.py:

```sh
git clone https://github.com/flatironinstitute/flathub
cd flathub/py
python setup.py install
```

Once you have successfully installed the module, you can use it as:

```
import flathub
```

## Usage

### Catalog list

You can get the available catalogs with `getCatalog`:

```python
catalogs = flathub.getCatalogs()
```

You can also get a single catalog by name:
```python
catalog = flathub.Catalog('gaiadr2')
```

Once you have a catalog, you can view the metadata and fields, and do queries:

```python
catalog.title
catalog['source_id']

stats = catalog.stats(fields = ['dec'], ra = (120, 130), parallax = (None, 4), phot_g_mean_flux = (100000, None))
stats['dec']['avg']

histogram = catalog.histogram(fields = ['parallax'], quartiles = 'phot_g_mean_flux', ra = (120, 130))
histogram.buckets

count = catalog.count(ra = (120, 130), parallax = (3, 4))
data = catalog.numpy(fields = ['source_id', 'ra', 'dec', 'parallax'], sample = 1000.0/count, ra = (120, 130), parallax = (3, 4))
```

## License

This project is licensed under the Apache 2 License - see the [COPYING](../COPYING) file for details.
