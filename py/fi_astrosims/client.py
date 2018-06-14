import json
import urllib.parse
import urllib.request
import numpy

__all__ = ['Simulation', 'Query']

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
    return urllib.request.urlopen(urllib.request.Request(url, headers=headers))

def getJSON(url):
    res = get(url, headers={'accept': 'application/json'})
    return json.loads(res.read().decode(res.info().get_content_charset('utf-8')))

class Simulation:
    """Reference to a particular catalog.

    *name* is the id of the catalog at the *host* url.

    The resulting object contains the JSON description of the *catalog*, and a dictionary of available *fields*.
    """
    def __init__(self, name, host = "http://astrosims.flatironinstitute.org"):
        self.url = urllib.parse.urljoin(host, name)
        self.catalog = getJSON(self.url)
        self.fields = { f['name']: f for f in self.catalog['fields'] }

class Query:
    def __init__(self, simulation, fields = None, sort = None, sample = 1, seed = None, **filters):
            if type(simulation) is str:
                simulation = Simulation(simulation)
            self.simulation = simulation
            if fields is None:
                fields = [ f['name'] for f in simulation.catalog['fields'] ]
            self.query = { 'fields': delim.join(fields) }
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
                if type(v) is list:
                    v = delim.join(map(str, v))
                else:
                    v = str(v)
                self.query[k] = v
            self._count = None
            self._aggs = {}

    def makeurl(self, path, **query):
        return makeurl(self.simulation.url, path, dictWith(self.query, **query))

    def count(self):
        if self._count is None:
            res = getJSON(self.makeurl('catalog', limit=0))
            self._count = res['hits']['total']
        return self._count

    def aggs(self, field):
        if field not in self._aggs:
            res = getJSON(self.makeurl('catalog', limit=0, aggs=field))
            self._count = res['hits']['total']
            self._aggs.update(res['aggregations'])
        return self._aggs[field]

    def avg(self, field):
        return self.aggs(field)['avg']

    def min(self, field):
        return self.aggs(field)['min']

    def max(self, field):
        return self.aggs(field)['max']

    def hist(self, field, width=None, bins=100):
        if not width:
            width = (self.max(field) - self.min(field))/bins
        res = getJSON(self.makeurl('catalog', limit=0, hist=field+':'+str(width)))
        return numpy.array([ (b['key'], b['doc_count']) for b in res['aggregations']['hist']['buckets'] ],
                [('bucket', self.simulation.fields[field]['dtype']), ('count', 'u8')])

    def numpy(self):
        return numpy.load(numpy.DataSource().open(self.makeurl('npy'), 'rb'))
