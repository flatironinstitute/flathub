#!/bin/env python

import argparse
import elasticsearch
import elasticsearch.helpers
import numpy
import os

argp = argparse.ArgumentParser(description="neutrino group catalog elastic search import")
argp.add_argument('-I', '--index', default='neutrino', help="Index name")
argp.add_argument('-T', '--type', default='catalog', help="Type name")
argp.add_argument('-i', '--instance', type=int, default=1, help="Start instance")
argp.add_argument('dir', nargs='+', help="Simulation directory")
args = argp.parse_args()

es = elasticsearch.Elasticsearch()

mapping = es.indices.get_mapping(index=args.index, doc_type=args.type)[args.index]['mappings'][args.type]['properties']

suffixes = {
    3: ['x','y','z'],
    6: ['gas','dm','unused','tracers','stars','bh']
}

float3 = numpy.dtype((numpy.float32,3))
float6 = numpy.dtype((numpy.float32,6))

def generate():
    for basedir in args.dir:
        times = numpy.loadtxt(os.path.join(os.path.dirname(basedir), "times.txt")).tolist()
        times.append(1)
        simname = os.path.basename(basedir)
        basedoc = {'_index':args.index,'_type':args.type,'simulation':simname,'Mnu':float(simname[0:simname.index('eV')])}

        inst = args.instance
        while True:
            instdir = os.path.join(basedir, '%d'%inst)
            if not os.path.isdir(instdir): break
            basedoc['instance'] = inst
            snaps = 0
            while True:
                snapdir = os.path.join(instdir, 'groups_%03d'%snaps)
                if not os.path.isdir(snapdir): break
                snaps += 1
            assert(snaps == len(times))
            for snapnum, time in enumerate(times):
                print(simname, inst, snapnum)
                snapdir = os.path.join(instdir, 'groups_%03d'%snapnum)
                basedoc['snapshot'] = snapnum
                basedoc['time'] = time
                basedoc['redshift'] = 1/time-1
                filei = 0
                basei = 0
                TotNgroups = 0
                Nfiles = 1
                while filei < Nfiles:
                    name = os.path.join(snapdir, 'group_tab_%03d.%d'%(snapnum, filei))
                    if not os.path.exists(name): break
                    f = open(name, 'rb')
		    Ngroups    = numpy.fromfile(f, dtype=numpy.int32,  count=1)[0]
		    TotNgroups = numpy.fromfile(f, dtype=numpy.int32,  count=1)[0]
		    Nids       = numpy.fromfile(f, dtype=numpy.int32,  count=1)[0]
		    TotNids    = numpy.fromfile(f, dtype=numpy.uint64, count=1)[0]
		    Nfiles     = numpy.fromfile(f, dtype=numpy.uint32, count=1)[0]
                    if not Ngroups: continue
                    data = []
                    def add_data(k, v):
                        if len(v.shape) > 1:
                            for j, s in enumerate(suffixes[v.shape[1]]):
                                data.append((k+'_'+s, v[:,j]))
                        else:
                            data.append((k, v))
		    add_data('Len',    numpy.fromfile(f,dtype=numpy.int32,count=Ngroups))
		    add_data('Offset', numpy.fromfile(f,dtype=numpy.int32,count=Ngroups))
		    add_data('Mass',   numpy.fromfile(f,dtype=numpy.float32,count=Ngroups))
		    add_data('Pos',    numpy.fromfile(f,dtype=float3,count=Ngroups))
		    add_data('Vel',    numpy.fromfile(f,dtype=float3,count=Ngroups))
		    add_data('LenType',   numpy.fromfile(f,dtype=float6,count=Ngroups))
		    add_data('MassType',  numpy.fromfile(f,dtype=float6,count=Ngroups))
                    baseid = "%s/%d/%03d/"%(simname,inst,snapnum)
                    doc = basedoc
                    for n, _ in data:
                        assert(mapping.has_key(n))
                    for i in xrange(Ngroups):
                        doc['_id'] = baseid + str(basei+i)
                        for n, v in data:
                            doc[n] = v[i].item()
                        yield doc
                    pos = f.tell()
                    f.seek(0,2)
                    assert(f.tell() == pos)
                    f.close()
                    basei += Ngroups
                    filei += 1
                assert(Nfiles == filei)
                assert(TotNgroups == basei)
            inst += 1

n = 0
for r in elasticsearch.helpers.streaming_bulk(es, generate()):
    n += 1
    if n % 10000 == 0:
        print(n)
print(n)
es.indices.refresh(index=args.index)
