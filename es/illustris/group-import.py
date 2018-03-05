#!/bin/env python

import argparse
import elasticsearch
import elasticsearch.helpers
import h5py
import numpy
import os

argp = argparse.ArgumentParser(description="illustris group catalog elastic search import")
argp.add_argument('-I', '--index', default='illustris', help="Index name")
argp.add_argument('-T', '--type', default='group', help="Type name")
argp.add_argument('dir', help="Simulation directory")
argp.add_argument('snapshot', nargs='+', type=int, help="Snapshots")
args = argp.parse_args()

es = elasticsearch.Elasticsearch()

mapping = es.indices.get_mapping(index=args.index, doc_type=args.type)[args.index]['mappings'][args.type]['properties']

suffixes = {
    3: ['x','y','z'],
    6: ['gas','dm','unused','tracers','stars','bh']
}

def generate():
    simname = os.path.basename(args.dir)
    basedoc = {'_index':args.index,'_type':args.type,'simulation':simname}
    for snapnum in args.snapshot:
        print(snapnum)
        basedoc['snapshot'] = snapnum
        baseid = "%s_%03d_"%(simname,snapnum)
        snapdir = os.path.join(args.dir, 'groups_%03d'%(snapnum))
        filei = 0
        basei = 0
        maxi = 0
        while True:
            name = os.path.join(snapdir, 'groups_%03d.%d.hdf5'%(snapnum, filei))
            if not os.path.exists(name): break
            with h5py.File(name, 'r') as f:
                header = f['Header'].attrs
                assert(simname == header['SimulationName'])
                assert(snapnum == header['SnapshotNumber'])
                assert(filei == header['Num_ThisFile'])
                basedoc['time'] = header['Time']
                basedoc['redshift'] = header['Redshift']
                g = f['Group']
                keys = []
                for k, v in g.iteritems():
                    n = k
                    v = g[k]
                    if n.startswith('Group'):
                        n = n[5:]
                    if n.startswith('_'):
                        n = n[1:]
                    if len(v.shape) > 1:
                        for j, s in enumerate(suffixes[v.shape[1]]):
                            keys.append((n+'_'+s, v[:,j]))
                    else:
                        if k == 'GroupFirstSub':
                            v = numpy.array(v).astype(numpy.int32)
                        keys.append((n, v))
                doc = basedoc.copy()
                for n, _ in keys:
                    assert(mapping.has_key(n))
                    doc[n] = None
                for i in xrange(0, header['Ngroups_ThisFile']):
                    doc['_id'] = baseid + str(basei+i)
                    for n, v in keys:
                        doc[n] = v[i].item()
                    yield doc
                basei += header['Ngroups_ThisFile']
                maxi = header['Ngroups_Total']
            filei += 1
        assert(maxi == basei)

n = 0
for r in elasticsearch.helpers.streaming_bulk(es, generate()):
    n += 1
    if n % 1000 == 0:
        print(n)
print(n)
es.indices.refresh(index=args.index)
