#!/bin/env python

import argparse
import h5py
import numpy
import psycopg2
import os
import struct
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

binary_header = struct.pack('!11sii', 'PGCOPY\n\377\r\n\0', 0, 0)
binary_footer = struct.pack('!h', -1)

binary_fmt = {
    'float64': 'd',
    'float32': 'f',
    'uint64': 'Q',
    'uint32': 'I'
}

class HDF5Stream(h5py.File):
    def __init__(self, idx, name, off):
        super(HDF5Stream, self).__init__(name, 'r')
        self.n = off
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
        self.fmt = '!H'
        self.args = [len(columns)]
        for (n, t) in columns:
            v = cols.pop(n)
            assert((self.count,) == v.shape)
            assert(t == types[v.dtype.name])
            self.data.append(v)
            fmt = binary_fmt[v.dtype.name]
            self.args.append(struct.calcsize(fmt))
            self.args.append(None)
            self.fmt += 'i' + fmt
        assert(not cols)
        self.rowlen = struct.calcsize(self.fmt)
        self.done = None

    def readline(self, size):
        if self.done is None:
            self.done = False
            return binary_header
        if self.done:
            return ''

        n = self.n
        block = min(max(int(size/self.rowlen), 1), self.count-n)
        if block <= 0:
            self.done = True
            return binary_footer
        sys.stdout.write("%d\r"%n)
        sys.stdout.flush()
        self.n += block

        args = self.args
        buf = ''
        for i in xrange(n,n+block):
            for j in xrange(args[0]):
                args[2+2*j] = self.data[j][i]
            buf += struct.pack(self.fmt, *args)
        return buf

    def read(self, size):
        return self.readline(size)

with pg.cursor() as pgc:
    cols=[ '"'+c[0]+'"' for c in columns ]
    idx = 0
    for name in args.file:
        with HDF5Stream(idx, name, max(0, args.offset-idx)) as stream:
            print(repr(idx) + " " + name + " " + str(stream.count))
            pgc.copy_expert('COPY ' + args.table + ' (' + ','.join(cols) + ') FROM STDIN (FORMAT binary)', stream, size = 65536)
            idx += stream.count
