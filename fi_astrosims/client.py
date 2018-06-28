import json
import urllib.parse
import urllib.request
import numpy
import pandas as pd
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D

__all__ = ['Simulation', 'Query', 'Dataset']

delim = ' '


def dictWith(a, **b):
    if b:
        a = a.copy()
        a.update(b)
    return a


def makeurl(url, path=None, query=None):
    if path:
        if not (path.startswith('/') or url.endswith('/')):
            url += '/'
        url += path
    if query:
        url += '?' + urllib.parse.urlencode(query)
    return url


def get(url, headers={}):
    """
    :param url: (string) url to JSON catalog
    :param headers:
    :return:
    Helper function used in getJSON
    """
    return urllib.request.urlopen(urllib.request.Request(url, headers=headers))


def getJSON(url):
    """
    :param: url: (string) url to JSON catalog
    :return: (dict) dictionary of JSON catalog
    Helper function used in Simulation initialization, count, aggs, and hist.
    """
    res = get(url, headers={'accept': 'application/json'})
    return json.loads(res.read().decode(res.info().get_content_charset('utf-8')))


class Simulation:
    """
    Reference to a particular catalog.
    *name* is the id of the catalog at the *host* url.
    The resulting object contains the JSON description of the *catalog*, and a
    dictionary of available *fields*.
    """
    def __init__(self, name, host = "http://astrosims.flatironinstitute.org"):
        self.url = urllib.parse.urljoin(host, name)
        self.catalog = getJSON(self.url)
        self.fields = { f['name']: f for f in self.catalog['fields'] }


class Query:
    """
    A query against a Simulation catalog.
    *simulation* specifies the Simulation object or name to run against.
    See *update* for other parameters.
    """
    def __init__(self, simulation, fields = None, sort = None, sample = 1, seed = None, **filters):
        if type(simulation) is str:
            simulation = Simulation(simulation)
        self.simulation = simulation
        self.query = dict()
        if fields is None:
            fields = [ f['name'] for f in simulation.catalog['fields'] ]
        if filters is not None:
            for key in filters:
                if 'transform' in simulation.fields[key]:
                    filters[simulation.fields[key]['org-field']] = \
                        tuple([x/simulation.fields[key]['scale'] for x in filters[key]])
                    filters.pop(key)
        self.update(fields, sort, sample, seed, **filters)

    def update(self, fields = None, sort = None, sample = None, seed = None, **filters):
        """
        :param fields: (list) specifies the list of fields to return,
        defaulting to all simulation fields.
        :param sort: (type) specifies the ordering of result objects, which may
        be the name of a single field, a field name prefixed with '-' for
        descending, or a list of sort fields.
        :param sample: (float) sets the fraction of results to return as a
        random subset(0,1].
        :param seed: (int) specifies the random seed for this selection, which
        defaults to a different random set each time. If sample is set to a
        value > 1, the value will be converted into a fraction within (0,1].
        Note that each item is independently selected with the sample fraction,
        so the result set may not be exactly this fraction of the original size.
        :param filters: (dict) parameters specify restrictions on fields to
        query by. Each parameter must be the name of a field in the catalog,
        and have the value of None to not filter on this field, a scalar to
        filter exact values, or tuple of lower- and upper-bounds to select
        items in that range.
        Sample is applied first, then the remaining filters.
        """

        # forces self.count() to return the max number of items in the catalog
        self._count = None
        self._aggs = {}

        self.count()
        if fields:
            self.query['fields'] = delim.join(fields)
        if type(sort) is list:
            self.query['sort'] = delim.join(sort)
        elif sort:
            self.query['sort'] = sort
        if sample > 1:
            sample = sample / self.count()
        if sample < 1:
            self.query['sample'] = str(sample)
            if seed is not None:
                self.query['sample'] += '@' + str(seed)
        for (k, v) in filters.items():
            if not k in self.simulation.fields:
                raise KeyError('No such field: ' + k)
            if v is None:
                try:
                    del self.query[k]
                except KeyError:
                    pass
                continue
            if type(v) is tuple:
                v = delim.join(map(str, v))
            else:
                v = str(v)
            self.query[k] = v

        # forces count to update to correct value next time it is called
        self._count = None

    def makeurl(self, path, **query):
        return makeurl(self.simulation.url, path, dictWith(self.query, **query))

    def count(self):
        """
        :return: (int) Number of objects in the query
        Can also obtain this value through aggs.
        """
        if self._count is None:
            res = getJSON(self.makeurl('catalog', limit=0))
            self._count = res['hits']['total']
        return self._count

    def aggs(self, field):
        """
        :param: field: (string) field you want to query
        :return: (dict) properties of the field aggregated into a dictionary
        If you are querying on a transformed (scaled) field, scale factor will
        adjust results accordingly."""
        temp_dict = {'scale': 1, 'transform': False, 'field': field}
        if field not in self._aggs:
            if 'transform' in self.simulation.fields[field]:
                temp_dict.update({'scale': self.simulation.fields[field]['scale'], 'transform': True,
                                  'field': self.simulation.fields[field]['org-field']})
            res = getJSON(self.makeurl('catalog', limit=0, aggs=temp_dict['field']))
            self._count = res['hits']['total']
            self._aggs.update(res['aggregations'])
        if temp_dict['transform']:
            self._aggs[field] = self._aggs[temp_dict['field']].copy()
            for key in self._aggs[field]:
                if key != 'count':
                    self._aggs[field].update({key: self._aggs[field][key]*temp_dict['scale']})
        return self._aggs[field]

    def avg(self, field):
        """
        :param: field: (string) field you want to query
        :return: (float) average value of that field
        """
        return self.aggs(field)['avg']

    def min(self, field):
        """
        :param: field: (string) field you want to query
        :return: (float) min value of that field
        """
        return self.aggs(field)['min']

    def max(self, field):
        """
        :param: field: (string) field you want to query
        :return: (float) max value of that field
        """
        return self.aggs(field)['max']

    def hist(self, field, width=None, bins=100):
        """
        :param field: (string) field that you want a histogram produced of
        :param width: (int) width of each bin, either specified or calculated
        :param bins: (int) number of bins, defaults to 100
        :return: dataframe (pandas) with index-able fields 'bucket', 'count'
        If you are querying on a transformed (scaled) field, scale factor will
        adjust results accordingly.
        """
        scale = 1
        if 'transform' in self.simulation.fields[field]:
            scale = self.simulation.fields[field]['scale']
            field = self.simulation.fields[field]['org-field']
        if not width:
            width = (self.max(field) - self.min(field))/bins
        res = getJSON(self.makeurl('catalog', limit=0, hist=field+':'+str(width)))
        nparray = numpy.array([ (b['key'], b['doc_count']) for b in res['aggregations']['hist']['buckets'] ],
                [('bucket', self.simulation.fields[field]['dtype']), ('count', 'u8')])
        nparray['bucket'] *= scale
        return pd.DataFrame(nparray)


class Dataset:
    """An object to store query results locally using a pandas dataframe."""
    def __init__(self, Query):
        """
        :param Query: Query object, see class Query() for more information
        Converts the data from the query into a pandas dataframe. Can index
        similarly but has greater versatility in terms of indexing / appending,
        as well as removes most need for server side functions.
        """
        self.df = pd.DataFrame(numpy.load(numpy.DataSource(None).open(Query.makeurl('npy'), 'rb')))
        self.simulation = Query.simulation
        self.query = Query.query

    def convert(self, field, new_name, scale_factor):
        """
        :param field: (string) field that you want to perform a multiplicative
        unit conversion on
        :param new_name: (string) name of the new field that is queryable
        :param scale_factor: (int) factor that you transform the field by
        """
        self.df = self.df.join(self.df[field].rename(new_name) * scale_factor)
        self.simulation.fields[new_name] = self.simulation.fields[field].copy()
        self.simulation.fields[new_name].update({'name': new_name, 'transform': True,
                                                 'org-field': field, 'scale': scale_factor})
        self.simulation.catalog['fields'].append(self.simulation.fields[new_name])

    def pdQuery(self, sample=None, **filters):
        """
        :return: temp_df: dataframe of new query (pandas)
        A dataframe version of Query. See Query.update for more details about
        parameters. Can take both integer and fractional values.
        """
        if sample is None:
            sample = len(self.df)
        if sample < 1:
            sample = int(numpy.floor(sample * len(self.df)))
        temp_df = self.df.sample(sample)
        for key in filters:
            temp_df = temp_df.loc[temp_df[key].between(filters[key][0], filters[key][1], inclusive=False)]
        return temp_df.reset_index().drop('index', 1)

    def aggs(self, field):
        """
        :param field: (string) field that you want to obtain information about
        :return: (dict) aggregation of the field parameters, similar to
        Query.aggs(field)
        """
        return{'max': self.df[field].max(), 'avg': self.df[field].mean(),
               'count': len(self.df[field]), 'min': self.df[field].min(),
               'sum': self.df[field].sum()}

    def view_pos_2D(self):
        """
        Prototype 2D plotting to view data. Defaults to x-y cartesian space.
        """
        pl = plt.scatter(self.df['Pos_x'], self.df['Pos_y'], color='k')
        pl.set_sizes(self.df['StellarMass'])
        plt.show()

    def view_pos_3D(self):
        """
        Prototype 3D plotting to view data. Defaults to x-y-z cartesian space.
        """
        fig = plt.figure()
        ax = Axes3D(fig)
        ax.scatter(xs=self.df['Pos_x'], ys=self.df['Pos_y'],
                   zs=self.df['Pos_z'], s=self.df['StellarMass'])
        plt.show()
