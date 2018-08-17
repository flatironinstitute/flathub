# Astrosims Query Module

A query module created to access any of the simulation catalogs hosted on astrosims.flatironinstitute.org.

## Getting Started

### Prerequisites

To install the module, you will need Python 3 (to utilize urllib.parse) and Numpy, preferably the latest releases. This check can be easily done on your command line.

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

Once you have succesfully istalled the module, you can now utilize it through the following command: 

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

## Authors

* **Dylan Simons** - *Majority of Functions* - [dylex](https://github.com/dylex)
* **Austen Gabrielpillai** - *Tweaks and Documentation* [aust427](https://github.com/aust427)

See also the list of [contributors](https://github.com/flatironinstitute/astrosims-reproto/graphs/contributors) who participated in this project.

## License

This project is licensed under the MIT License - see the [LICENSE.md](LICENSE.md) file for details
