#!/bin/env python

import argparse
import elasticsearch
import elasticsearch.helpers
import h5py
import numpy
import os

argp = argparse.ArgumentParser(description="gaea catalog elastic search import")
argp.add_argument('-I', '--index', default='gaea', help="Index name")
argp.add_argument('-T', '--type', default='catalog', help="Type name")
argp.add_argument('-o', '--offset', type=int, default=0, help="Initial starting offset in first file")
argp.add_argument('file', nargs='+', help="Simulation files")
args = argp.parse_args()

es = elasticsearch.Elasticsearch(timeout=120)

mapping = es.indices.get_mapping(index=args.index, doc_type=args.type)[args.index]['mappings'][args.type]['properties']

suffixes = {
    2: ['x','y'],
    3: ['x','y','z'],
}

types = {
    'float64': 'double',
    'float32': 'float',
    'uint64': 'long',
    'uint32': 'integer',
}

def generate():
    basedoc = {'_index':args.index,'_type':args.type}
    for name in args.file:
        base, _ = os.path.splitext(os.path.basename(name))
        baseid = "%s_"%(base)
        print(base)
        with h5py.File(name, 'r') as f:
            keys = []
            for k, v in f.iteritems():
                if len(v.shape) > 1:
                    for j, s in enumerate(suffixes[v.shape[1]]):
                        keys.append((k+'_'+s, v[:,j]))
                else:
                    keys.append((k, v))
            doc = basedoc
            for n, v in keys:
                assert(mapping[n]['type'] == types[v.dtype.name])
                doc[n] = None
            count = keys[0][1].shape[0]
            if args.offset > count:
                args.offset -= count
                continue
            for i in xrange(args.offset, count):
                doc['_id'] = baseid + str(i)
                for n, v in keys:
                    doc[n] = v[i].item()
                yield doc
            args.offset = 0

n = 0
for r in elasticsearch.helpers.streaming_bulk(es, generate()):
    n += 1
    if n % 50000 == 0:
        print(n)
print(n)
es.indices.refresh(index=args.index)
