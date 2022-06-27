from typing import Any, Optional, List, Dict, Union, Tuple, TypedDict, Sequence

import json
import numpy
import requests
import urllib.parse

__all__ = ['Catalog', 'getCatalogs', 'Filters', 'ValueFilter', 'RangeFilter', 'WildcardFilter', 'StatsRes', 'NumericStats', 'TermsStats', 'TopTerm', 'SortField', 'HistogramField', 'HistogramRes']

delim = ' '

def urljoin(a: str, b: str) -> str:
    if not a.endswith('/'):
        a += '/'
    return urllib.parse.urljoin(a, b)

def queryValue(a) -> str:
    if type(a) is str:
        return a
    if a is None:
        return ''
    return json.dumps(a)

defaultEndpoint = "http://flathub.flatironinstitute.org/api"

def jsonResponse(res: requests.Response):
    if res.status_code >= 400:
        if res.headers['Content-type'] == 'text/plain;charset=utf-8':
            res.reason = res.content.decode('utf-8')
        res.raise_for_status()
    return res.json()

def GET(url: str):
    return jsonResponse(requests.get(url))

def POST(url: str, json):
    return jsonResponse(requests.post(url, json=json))

class Field(TypedDict):
    name: str
    key: str
    title: str
    type: str
    base: str
    dtype: str

FieldRef = Union[str, Field]

def fieldName(f: FieldRef) -> str:
    if isinstance(f, str):
        return f
    return f['name']

class Argument:
    @classmethod
    def make(cls, arg, *args, **kwargs):
        if isinstance(arg, cls):
            return arg
        if type(arg) == tuple:
            return cls(*arg, *args, **kwargs)
        return cls(arg, *args, **kwargs)

class ArgumentDict(Argument, Dict):
    def __getattr__(self, attr):
        if attr in self:
            return self[attr]
        return self.__getattribute__(attr)

class Filter:
    """Abstract Field filter.  Use ValueFilter, RangeFilter, or WildcardFilter."""
    def __init__(self, field: FieldRef):
        self.field = fieldName(field)
        self.filter: Any = None

    @classmethod
    def make(cls, field: FieldRef, arg):
        if isinstance(arg, cls):
            return arg
        if type(arg) == tuple and len(arg) == 2:
            return RangeFilter(field, *arg)
        if type(arg) == list:
            return ValueFilter(field, *arg)
        return ValueFilter(field, arg)

    def json(self):
        return self.filter

    def query(self) -> str:
        return queryValue(self.filter)

class ValueFilter(Filter):
    """Filter on a field having a specific value, or one of a set of values."""
    def __init__(self, field: FieldRef, *values):
        super().__init__(field)
        if len(values) > 1:
            self.filter = list(values)
        else:
            self.filter = values[0]


class RangeFilter(Filter):
    """Filter on a field being in some range: lte <= field <= gte
    Requires a numeric field."""
    def __init__(self, field: FieldRef, gte=None, lte=None):
        super().__init__(field)
        self.filter = {}
        if gte is not None:
            self.filter['gte'] = gte
        if lte is not None:
            self.filter['lte'] = lte

    def query(self) -> str:
        return ','.join(map(queryValue, (self.filter['gte'], self.filter['lte'])))

class WildcardFilter(Filter):
    """Filter on a string field matching a wildcard.  Requires a wildcard field."""
    def __init__(self, field: FieldRef, wildcard: str):
        super().__init__(field)
        self.filter = {'wildcard': wildcard}

class Filters(Argument):
    """A set of row filters.
    In most cases where functions take a filters argument, they can
    alternatively be passed the same arguments as the Filters constructor
    directly instead.
    """
    def __init__(self, sample: float=1.0, seed: Optional[int] = None, *filters: Filter, **field_filters):
        """Construct a set of Filters.

        Parameters:
            sample: float, random fractional sample (0 < sample <= 1)
            seed: int, seed for the random sample (to ensure consistent results -- None means random)
            filters: a collection of field Filter objects, OR
            field_filters: a set of filters by field name, each of which may be
                (gte, lte) for a RangeFilter
                [value_list] for a ValueFilter list
                value for a scalar ValueFilter
        """
        self.sample = sample
        self.seed = seed
        self.filters: Dict[str,Filter] = {}
        self += filters
        self.update(field_filters)

    def __iadd__(self, arg: Union[Filter,Sequence[Filter]]):
        """Add one or more Filter objects to these Filters."""
        if isinstance(arg, Filter):
            arg = [arg]
        for f in arg:
            if not isinstance(f, Filter):
                raise TypeError(f"Filters += expected Filter: {arg}")
            self.filters[f.field] = f
        return self

    def __setitem__(self, field: FieldRef, filt):
        """Set a filter for a field."""
        self += Filter.make(field, filt)

    def update(self, other: Dict[str, Any]):
        for n, v in other.items():
            self[n] = v

    def __getitem__(self, field: FieldRef) -> Filter:
        return self.filters[fieldName(field)]

    def json(self) -> Dict[str,Any]:
        r = {}
        if self.sample < 1:
            r['sample'] = self.sample
            if self.seed is not None:
                r['seed'] = self.seed
        for f in self.filters.values():
            r[f.field] = f.json()
        return r

    def query(self) -> Dict[str,str]:
        r = {}
        if self.sample < 1:
            r['sample'] = str(self.sample)
            if self.seed is not None:
                r['seed'] = str(self.seed)
        for f in self.filters.values():
            r[f.field] = f.query()
        return r

class NumericStats(TypedDict):
    """Stats for numeric fields.

    Attributes:
        count: int, number of rows with a value for this field
        min, max, avg
    """
    count: int
    min: float
    max: float
    avg: float

class TopTerm(TypedDict):
    count: int
    value: Any

class TermsStats(TypedDict):
    """Stats for non-numeric fields.

    Attributes:
        terms: List[TopTerm], counts for the top values
        others: int, number of rows with values not incuded in the top terms
    """
    terms: List[TopTerm]
    others: int

class StatsRes:
    """Result of a Catalog.stats call."""
    def __init__(self, res):
        self.json = res

    def __getitem__(self, field: FieldRef) -> Union[NumericStats, TermsStats]:
        """Get stats for a particular field."""
        return self.json[fieldName(field)]

    def __repr__(self) -> str:
        return repr(self.json)

class SortField(Argument):
    """Specification for sort order by a particular field."""
    def __init__(self, field: FieldRef, asc: bool = True):
        self.field = fieldName(field)
        self.asc = asc

    def json(self) -> Union[str, Dict[str, str]]:
        if self.asc:
            return self.field
        else:
            return {'field': self.field, 'order': 'desc'}

    def query(self) -> str:
        if self.asc:
            return self.field
        else:
            return '-'+self.field

class HistogramField(ArgumentDict):
    """Specification for a field on which to compute a histogram."""
    def __init__(self, field: FieldRef, size: int=16, log: bool=False, catalog=None):
        self.field = catalog[field] if type(field) is str else field
        self['field'] = self.field['name']
        self['size'] = size
        self.log = log
        if log:
            self['log'] = log

    def json(self) -> Dict[str, Any]:
        return self

class HistogramRes:
    """Result of a Catalog.histogram call."""
    def __init__(self, fields: List[HistogramField], quartiles: Optional[Field], json):
        self.fields = fields
        self.quartiles = quartiles
        self.json = json

    @property
    def sizes(self) -> numpy.ndarray:
        """The size of a bucket in each dimension."""
        dtypes = [(f.field['name'], f.field['dtype']) for f in self.fields]
        return numpy.array([tuple(self.json['sizes'])], dtypes)

    @property
    def buckets(self) -> numpy.ndarray:
        """A numpy array of the buckets, containing key values, count, and field quartiles.
        Each the bucket covers the range [key,key+size) (or [key,key*size) for log scale)."
        """
        dtypes = [(f.field['name'], f.field['dtype']) for f in self.fields]
        dtypes.append(('count', 'u8'))
        if self.quartiles:
            dtypes.extend([(self.quartiles['name']+'.'+q, self.quartiles['dtype']) for q in ['min','q1','median','q3','max']])
        def mk(r):
            d = r['key'].copy()
            d.append(r['count'])
            if self.quartiles:
                d.extend(r['quartiles'])
            return tuple(d)
        return numpy.array(list(map(mk, self.json['buckets'])), dtypes)

    def __repr__(self) -> str:
        return repr(self.json)

class Catalog:
    """
    Reference to a particular catalog.
    The resulting object contains the JSON description of the *catalog*, and a
    dictionary of available *fields*.
    """
    def __init__(self, info: Union[str, Dict[str,Any]], endpoint = defaultEndpoint):
        if isinstance(info, str):
            self.name = info
            self._catalog: Dict[str,Any] = {}
        else:
            self.name = info['name']
            self._catalog = info
        self.endpoint = urljoin(endpoint, self.name)
        self._fieldDict: Optional[Dict[str,Field]] = None

    @property
    def catalog(self):
        """The JSON metadata about this catalog."""
        if 'count' not in self._catalog:
            self._catalog = GET(self.endpoint)
        return self._catalog

    def __getattr__(self, attr):
        if attr in self._catalog or attr in self.catalog:
            return self._catalog[attr]
        return self.__getattribute__(attr)

    def __str__(self):
        return self.name

    def __repr__(self):
        return f'Catalog({self.name})'

    @property
    def fieldDict(self) -> Dict[str,Field]:
        """A dictionary of fields in this catalog."""
        if self._fieldDict is not None:
            return self._fieldDict
        d = {}
        def add(fl):
            for f in fl:
                if 'sub' in f:
                    add(f['sub'])
                else:
                    d[f['name']] = f
        add(self.fields)
        self._fieldDict = d
        return d

    def __getitem__(self, name: str) -> Field:
        """Lookup a field by name."""
        return self.fieldDict[name]

    def count(self, filters: Optional[Filters] = None, *args, **kwargs) -> int:
        """Return a count of rows matching the given Filters."""
        if not filters:
            filters = Filters(*args, **kwargs)
        body = filters.json()
        return POST(urljoin(self.endpoint, 'count'), body)
    
    def stats(self, fields: List[FieldRef], filters: Optional[Filters] = None, *args, **kwargs) -> StatsRes:
        """Return stats for the selected fields given some filters."""
        if not filters:
            filters = Filters(*args, **kwargs)
        body = filters.json()
        body['fields'] = list(map(fieldName, fields))
        return StatsRes(POST(urljoin(self.endpoint, 'stats'), body))

    def histogram(self, filters: Optional[Filters] = None, fields: List[Union[HistogramField, FieldRef, Tuple[FieldRef, int], Tuple[FieldRef, int, bool]]] = [], quartiles: Optional[FieldRef]=None, *args, **kwargs) -> HistogramRes:
        """Return a histogram for the selected fields given some filters."""
        if not filters:
            filters = Filters(*args, **kwargs)
        body = filters.json()
        body['fields'] = [HistogramField.make(h, catalog=self) for h in fields]
        if quartiles:
            body['quartiles'] = fieldName(quartiles)
        return HistogramRes(
                body['fields'],
                self[body['quartiles']] if quartiles else None,
                POST(urljoin(self.endpoint, 'histogram'), body))

    def numpy(self, fields: List[FieldRef], filters: Optional[Filters] = None, sort: List[Union[FieldRef, Tuple[FieldRef, bool], SortField]] = [], destpath = None, *args, **kwargs) -> numpy.ndarray:
        """Returns matching rows in numpy format.
        Downloads files to destpath as per numpy.DataSource."""
        if not filters:
            filters = Filters(*args, **kwargs)
        query = filters.query()
        query['fields'] = ','.join(map(fieldName, fields))
        query['sort'] = ','.join(SortField.make(s).query() for s in sort)
        url = urljoin(self.endpoint, 'data/npy')+'?'+urllib.parse.urlencode(query)
        return numpy.load(numpy.DataSource(destpath).open(url, 'rb'))

def getCatalogs(endpoint: str = defaultEndpoint) -> Dict[str, Catalog]:
    """
    Get the list of available catalogs as Catalog objects.
    """
    cats = GET(endpoint)
    return {c['name']: Catalog(c, endpoint) for c in cats}
