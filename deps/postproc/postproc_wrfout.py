#!/usr/bin/env python

import sys
import time
import argparse
import string
import os
from wrf_model_data import WRFModelData
import matplotlib.pyplot as plt
import numpy as np
#import riak

def render_to_png(fig, data, caxis, filename):
    fig.figimage(data, alpha=0.3, vmin=caxis[0], vmax=caxis[1])
    fig.savefig(filename)


if __name__ == '__main__':

    parser = argparse.ArgumentParser(description='Generates png images from wrfout files.')
    parser.add_argument('wrfout_file', type=str, help='the wrfout file to process')
    parser.add_argument('output_dir', type=str, help='the output directory for the PNG files')
    parser.add_argument('-v', '--vars', metavar='V', type=str, dest='varlst', default=None, help='variables to extract, default: T2,PSFC,RAINC,RAINNC')
    args = parser.parse_args()

    varlst = args.varlst or 'T2,PSFC,Q2'
    varlst = string.split(varlst, ',')
    print("Processing variables %s" % str(varlst))
    
    w = WRFModelData(args.wrfout_file, varlst)
    ts = w['GMT']

    # create a default shape figure
    S = w.get_lons().shape
    dpi = 120.0
    fig = plt.figure(figsize = (S[1]/dpi, S[0]/dpi), dpi = dpi)
    print("Found %d times in wrfout." % len(ts))

    caxis = (np.amin(w['T2']), np.amax(w['T2']))

    for vname in ['T2']:
        data = w[vname]
        for t in range(24):
            print("Processing time %s (%d/%d) ..." % (str(ts[t]), t+1, len(ts)))
            ts_t = ts[t]
            render_to_png(fig,
                          data[t, :, :],
                          caxis,
                          os.path.join(args.output_dir, vname + "_" + ts_t.strftime("%Y-%m-%dT%H:%M:00") + ".png"))


