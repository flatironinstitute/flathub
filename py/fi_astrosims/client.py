import json
try:
    from urllib.parse import urlencode, urljoin
    from urllib.request import urlopen, Request
except ImportError:
    from urllib import urlencode, basejoin as urljoin
    from urllib2 import urlopen, Request
import numpy
import random

__all__ = ['Simulation', 'Query', 'getCatalogs']

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
        url += '?' + urlencode(query)
    return url


def get(url, headers={}):
    """
    :param url: (string) url to JSON catalog
    :param headers:
    :return:
    Helper function used in getJSON
    """
    return urlopen(Request(url, headers=headers))


def getJSON(url):
    """
    :param: url: (string) url to JSON catalog
    :return: (dict) dictionary of JSON catalog
    Helper function used in Simulation initialization, count, aggs, and hist.
    """
    res = get(url, headers={'accept': 'application/json'})
    data = res.read()
    if type(data) is not str:
        data = data.decode(res.info().get_content_charset('utf-8'))
    return json.loads(data)

defaultHost = "http://astrosims.flatironinstitute.org"
catalogs = None

def getCatalogs(host = defaultHost):
    """
    :return: (dict) dictionary of hosted Catalogs
    Function to create a dict of available catalogs for querying. 
    """
    if not catalogs:
        catalogs = getJSON(host)
    return catalogs

class Simulation:
    """
    Reference to a particular catalog.
    *name* is the id of the catalog at the *host* url.
    The resulting object contains the JSON description of the *catalog*, and a
    dictionary of available *fields*.
    """
    def __init__(self, name, host = defaultHost):
        self.url = urljoin(host, name)
        self.catalog = getJSON(self.url)
        self.fields = { f['name']: f for f in self.catalog['fields'] }

    def query(self, **kwargs):
        """Construct a Query using the given arguments and this simulation."""
        return Query(self, **kwargs)

class Query:
    """
    A query against a Simulation catalog.
    *simulation* specifies the Simulation object or name to run against.
    See *update* for other parameters.
    """
    def __init__(self, simulation, fields = None, sort = None, sample = 1, seed = None, **filters):
        if seed is None:
            seed = random.randrange(0, 2**32-1)
        if type(simulation) is str:
            simulation = Simulation(simulation)
        self.simulation = simulation
        self.query = dict()
        if fields is None:
            fields = [ f['name'] for f in simulation.catalog['fields'] ]
        if sample > 1:
            self.update(fields, sort, sample, seed, **filters)
            sample = sample / self.count()
        self.update(fields, sort, sample, seed, **filters)

    def update(self, fields = None, sort = None, sample = None, seed = None, **filters):
        """
        :param fields: (list) specifies the list of fields to return,
        defaulting to all simulation fields.
        :param sort: (type) specifies the ordering of result objects, which may
        be the name of a single field, a field name prefixed with '-' for
        descending, or a list of sort fields.
        :param sample: (float) sets the fraction of results to return as a
        random subset(0,1]. If sample is set to a value > 1, the value will be
        converted into a fraction within (0,1].
        :param seed: (int) specifies the random seed for this selection, which
        defaults to a different random set each time. If seed is None, a random
        seed is assigned.
        Note that each item is independently selected with the sample fraction,
        so the result set may not be exactly this fraction of the original size.
        :param filters: (dict) parameters specify restrictions on fields to
        query by. Each parameter must be the name of a field in the catalog,
        and have the value of None to not filter on this field, a scalar to
        filter exact values, or tuple of lower- and upper-bounds to select
        items in that range.
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
        """
        if self._count is None:
            res = getJSON(self.makeurl('catalog', limit=0))
            self._count = res['hits']['total']
        return self._count

    def aggs(self, field):
        """
        :param: field: (string) field you want to query
        :return: (dict) properties of the field aggregated into a dictionary
        """
        if field not in self._aggs:
            res = getJSON(self.makeurl('catalog', limit=0, aggs=field))
            self._count = res['hits']['total']
            self._aggs.update(res['aggregations'])
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
        :param width: (float) width of each bin, either specified or calculated
        :param bins: (int) number of bins, defaults to 100
        :return: dataframe (pandas) with index-able fields 'bucket', 'count'
        """

        res = getJSON(self.makeurl('catalog', limit=0, hist=field+':'+str(bins)))
        print(res)
        return numpy.array([ (b['key'], b['doc_count']) for b in res['aggregations']['hist']['buckets'] ],
                [('bucket', self.simulation.fields[field]['dtype']), ('count', 'u8')])

    def numpy(self):
        """
        :return: array (numpy void)
        Returns the data from the query in the form of a numpy void array.
        """
        return numpy.load(numpy.DataSource(None).open(self.makeurl('npy'), 'rb'))

