#!/usr/bin/env python

import sys
import time
import argparse
import string
import os
import numpy as np
import pytz
import riak
import shapefile

import matplotlib.pyplot as plt
from matplotlib.collections import LineCollection
from mpl_toolkits.basemap import Basemap

from wrf_model_data import WRFModelData


def render_to_png(fig, m, data, lats, lons, mx, my, counties, caxis, title, fname):

    # prep figure for reuse
    fig.clf()
    ax = plt.subplot(111)

    # retrieve geographical limits
    x1, x2 = np.amin(lons), np.amax(lons)
    y1, y2 = np.amin(lats), np.amax(lats)

    # paint the image
    m.pcolormesh(mx, my, data, shading='gouraud')

    # draw parallels/meridians
    m.drawparallels(np.arange(int(y1), int(y2)+1, 2.), labels=[1,0,0,0], color='black',
                    dashes=[1,0], labelstyle='+/-', linewidth=0.4)
    m.drawmeridians(np.arange(int(x1), int(x2)+1 ,2.), labels=[0,0,0,1], color='black',
                    dashes=[1,0], labelstyle='+/-', linewidth=0.2)

    # draw all the counties
    for county in counties:
        ax.add_collection(county)

    # add title & colorbar
    plt.title(title)
    plt.clim(caxis)
    plt.colorbar()

    fig.savefig(fname)


def load_colorado_shapes(m):

    # read all US counties
    rdr = shapefile.Reader("../USA_adm/USA_adm2")
    shapes = rdr.shapes()
    records = rdr.records()

    # only keep Colorado counties
    ndx = filter(lambda i: records[i][4] == 'Colorado', np.arange(len(shapes)))
    shapes = [shapes[i] for i in ndx]
    records = [records[i] for i in ndx]

    # modified from web tutorial
    # http://www.geophysique.be/2013/02/12/matplotlib-basemap-tutorial-10-shapefiles-unleached-continued/
    line_col = []
    for record, shape in zip(records, shapes):
        lons,lats = zip(*shape.points)
        data = np.array(m(lons, lats)).T
 
        if len(shape.parts) == 1:
            segs = [data,]
        else:
            segs = []
            for i in range(1,len(shape.parts)):
                index = shape.parts[i-1]
                index2 = shape.parts[i]
                segs.append(data[index:index2])
                segs.append(data[index2:])
 
        lines = LineCollection(segs, antialiaseds=(1,))
        lines.set_edgecolors('k')
        lines.set_linewidth(0.8)
        line_col.append(lines)

    return line_col


def setup_basemap_proj(lats, lons):

    x1, x2 = np.amin(lons), np.amax(lons)
    y1, y2 = np.amin(lats), np.amax(lats)

    # assume the mercator projection is suitable (restricts domains)
    m = Basemap(resolution='i',
                projection='merc',
                llcrnrlat=y1,urcrnrlat=y2,
                llcrnrlon=x1,urcrnrlon=x2,
                lat_ts=(y1+y2)/2)

    return m


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description='Generates png images from wrfout files.')
    parser.add_argument('wrfout_file', type=str, help='the wrfout file to process')
    parser.add_argument('output_dir', type=str, help='the output directory for the PNG files')
    parser.add_argument('-v', '--vars', metavar='V', type=str, dest='varlst', default=None, help='variables to extract, default: T2,PSFC,RAINC,RAINNC')
    parser.add_argument('-b', '--bucket', metavar='B', type=str, dest='bucket', help='bucket under which to save the images')
    parser.add_argument('-n', '--hostname', metavar = 'N', type=str, dest='riak_host', help='riak hostname')
    parser.add_argument('-p', '--port', metavar = 'P', type=int, dest='riak_port', help='riak port accepting pbc transactions')
    args = parser.parse_args()

    varlst = args.varlst or 'T2,PSFC,Q2,RAINC,RAINNC'
    varlst = string.split(varlst, ',')

    print("INFO: loading wrf model output data ...")
    w = WRFModelData(args.wrfout_file, varlst)
    ts = w['GMT']

    if 'RAINC' in varlst and 'RAINNC' in varlst:
        varlst.remove('RAINC')
        varlst.remove('RAINNC')
        varlst.append('RAIN')

    print("INFO: processing variables %s" % str(varlst))

    # create a default shape figure and basemap projection
    print("INFO: preparing projection ...")
    lats, lons = w.get_lats(), w.get_lons()
    fig = plt.figure(figsize = (10, 6))
    m = setup_basemap_proj(lats, lons)
    mx, my = m(lons, lats)

    print("INFO: loading county shapes ...")
    counties = load_colorado_shapes(m)

    print("INFO: found %d times in wrfout." % len(ts))

    mst = pytz.timezone("US/Mountain")
    gmt = pytz.timezone("GMT")

    # loop through variables requested
    for vname in varlst:
        print("INFO: processing variable %s ..." % vname)
        data = w[vname]
        if vname == 'T2':
            # convert to Fahrenheit
            data = (data - 273.15) * 9.0 / 5 + 32.0

        caxis = (np.amin(data), np.amax(data))
        for t in range(len(ts)):
            print("Processing time %s (%d/%d) ..." % (str(ts[t]), t+1, len(ts)))
            ts_t = ts[t]
            ts_str = ts_t.strftime("%Y-%m-%d_%H:%M:00")
            ts_mst_str = ts_t.astimezone(mst).strftime("%Y-%m-%d %H:%M:00")
            render_to_png(fig,
                          m,
                          data[t, :, :],
                          lats, lons,
                          mx, my,
                          counties,
                          caxis,
                          "%s at %s" % (vname, ts_mst_str),
                          os.path.join(args.output_dir, vname + "_" + ts_str + ".png"))


