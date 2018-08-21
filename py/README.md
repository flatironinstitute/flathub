# Astrosims Query Module

A query module created to access any of the simulation catalogs hosted on astrosims.flatironinstitute.org.

## Getting Started

### Prerequisites

To install the module, you will need Python 3 and numpy, preferably the latest releases. This check can be easily done on your command line.

```
python3 -v
'Python 3.5.2'
```

```
import numpy
numpy.version.version
'1.14.5'
```

### Installing

To install, just clone this repository and run setup.py:

```
git clone git://github.com/flatironinstitute/astrosims-reproto
cd astrosims-reproto/py
python3 setup.py install
```

Once you have successfully installed the module, you can use it as:

```
import fi_astrosims.client 
```

## Running the Module 

To start off, create a Simulation object based off the simulation you want to query. From there, you can create a Query object and can query based off fields, sample, seed, and sort). 

```
gaea = fi_astrosims.client.Simulation('gaea') 
q = fi_astrosims.client.Query(gaea, Mvir = (4.5, 4900), seed = 0, sample = 0.5) 
```

If you want to view some basic statistics about your query, run either the count() or aggs() functions. 

```
q.count()
q.aggs('Mvir')
```

Finally, you can download your query locally using the numpy() function, which will download the query as a .npy file. 

```
x = q.numpy()
```

## License

This project is licensed under the Apache 2 License - see the [COPYING](../COPYING) file for details.
