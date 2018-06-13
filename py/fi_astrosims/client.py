import json
import urllib.parse
import urllib.request

delim = ' '

class Simulation:
    def __init__(self, name, host = "http://astrosims.flatironinstitute.org"):
        self.url = urllib.parse.urljoin(host, name)
        res = urllib.request.urlopen(urllib.request.Request(self.url, headers={'accept': 'application/json'}))
        self.catalog = json.loads(res.read().decode(res.info().get_content_charset('utf-8')))
        self.fields = { f['name']: f for f in self.catalog['fields'] }

class Query:
    def __init__(self, simulation, fields = None, sort = None, sample = 1, seed = None, **filters):
            if type(simulation) is str:
                simulation = Simulation(simulation)
            self.simulation = simulation
            if fields is None:
                fields = [ f['name'] for f in simulation.catalog['fields'] ]
            query = { 'fields': delim.join(fields) }
            if type(sort) is list:
                query['sort'] = delim.join(sort)
            elif sort:
                query['sort'] = sort
            if sample < 1:
                query['sample'] = str(sample)
                if seed is not None:
                    query['sample'] += '@' + str(seed)
            for (k, v) in filters.items():
                if not k in self.simulation.fields:
                    raise KeyError('No such field: ' + k)
                if type(v) is list:
                    v = delim.join(map(str, v))
                else:
                    v = str(v)
                query[k] = v
            self.query = urllib.parse.urlencode(query)

