#!/bin/env python

import argparse
import h5py
import numpy
import psycopg2
import os
import sys

argp = argparse.ArgumentParser(description="gaea catalog pgsql import")
argp.add_argument('-D', '--database', default='astrosims', help="Database name")
argp.add_argument('-T', '--table', default='gaea', help="Table name")
argp.add_argument('-o', '--offset', type=int, default=0, help="Initial starting offset")
argp.add_argument('file', nargs='+', help="Simulation files")
args = argp.parse_args()

pg = psycopg2.connect(dbname=args.database)
pg.autocommit = True

with pg.cursor() as pgc:
    pgc.execute("SELECT column_name, data_type FROM information_schema.columns WHERE table_name = %s", (args.table, ))
    columns = pgc.fetchall()

suffixes = {
    2: ['x','y'],
    3: ['x','y','z'],
}

types = {
    'float64': 'double precision',
    'float32': 'real',
    'uint64': 'bigint',
    'uint32': 'integer',
}

SEP = '\t'

class HDF5Stream(h5py.File):
    def __init__(self, idx, name, off):
        super(HDF5Stream, self).__init__(name, 'r')
        self.i = off
        cols = {}
        for k, v in self.iteritems():
            if len(v.shape) > 1:
                for j, s in enumerate(suffixes[v.shape[1]]):
                    cols[k+'_'+s] = v[:,j]
            else:
                cols[k] = v
        self.count, = cols.values()[0].shape
        cols['_id'] = numpy.arange(idx, idx+self.count, dtype='uint64')
        self.data = []
        for (n, t) in columns:
            v = cols.pop(n)
            assert((self.count,) == v.shape)
            assert(t == types[v.dtype.name])
            self.data.append(v)
        assert(not cols)

    def readline(self, size):
        i = self.i
        if i >= self.count: return ''
        if i % 100 == 0:
            sys.stdout.write("%d\r"%i)
            sys.stdout.flush()
        self.i += 1
        row = SEP.join(repr(v[i]) for v in self.data)
        return row+'\n'

    def read(self, size):
        return self.readline(size)

with pg.cursor() as pgc:
    cols=[ '"'+c[0]+'"' for c in columns ]
    idx = 0
    for name in args.file:
        with HDF5Stream(idx, name, max(0, args.offset-idx)) as stream:
            print(repr(idx) + " " + name + " " + str(stream.count))
            pgc.copy_from(stream, args.table, sep=SEP, columns=cols)
            idx += stream.count
