#!/usr/bin/env python

import sys
import time
import argparse
import string
import os
import numpy as np
import pytz
import riak
import glob


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description='Uploads png files into riak cluster according to wildcard.')
    parser.add_argument('wildcard', type=str, help='the wildcard of files to be uploaded')
    parser.add_argument('-b', '--bucket', metavar='B', type=str, dest='bucket', help='bucket under which to save the images')
    parser.add_argument('-n', '--hostname', metavar = 'N', type=str, dest='riak_host', help='riak hostname')
    parser.add_argument('-p', '--port', metavar = 'P', type=int, dest='riak_port', help='riak port accepting pbc transactions')
    args = parser.parse_args()

    files = glob.glob(args.wildcard)
    N = len(files)
    print("INFO: found %d files ..." % N)

    print("INFO: setting up RIAK client")
    client = riak.RiakClient(host = args.riak_host,
                             pb_port = args.riak_port,
                             protocol = 'pbc')
    bucket = client.bucket(args.bucket)


    ndx = 1
    for fname in files:
        print("Uploading %s (%d/%d) ..." % (fname, ndx, N))
        riak_key = fname.split('.')[0]

        # read in the file
        with open(fname, 'rb') as f:
                fig_data = f.read()

        # store it in RIAK
        new_fig = bucket.new(riak_key,
                             encoded_data = fig_data,
                             content_type = 'image/png')
        new_fig.store()
        ndx += 1


