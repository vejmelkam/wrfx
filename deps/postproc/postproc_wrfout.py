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
from multiprocessing import Pool, Queue, Process

# use agg renderer, suitable for raster graphics
import matplotlib
matplotlib.use("agg")

import matplotlib.pyplot as plt
from matplotlib.collections import LineCollection
from mpl_toolkits.basemap import Basemap

from wrf_model_data import WRFModelData


RenderConfig = {

    'T2' : { 'caxis' : None, 'cmap' : 'jet' },
    'RH' : { 'caxis' : (0.0, 100.0), 'cmap' : 'jet_r' },
    'RAIN' : { 'caxis' : None, 'cmap' : 'Greys' },
    'FM1' : { 'caxis' : (0.0, 30.0), 'cmap' : 'jet_r' },
    'FM10' : { 'caxis' : (0.0, 30.0), 'cmap' : 'jet_r' },
    'FM100' : { 'caxis' : (0.0, 30.0), 'cmap' : 'jet_r' },
    'F_ROS' : { 'caxis' : None, 'cmap' : 'jet' },
    'F_LINEINT2' : { 'caxis' : None, 'cmap' : 'jet' }

}


def postproc_worker(lats, lons, args, counties, jobs, i):

    done = 0

    fig = plt.figure(figsize = (8, 5))
    fig.subplots_adjust(left=0.1, right = 1.0, top = 1.0, bottom = 0.05)

    print("WORKER [%d]: setting up RIAK client" % i)
    client = riak.RiakClient(host = args.riak_host,
                             pb_port = args.riak_port,
                             protocol = 'pbc')
    bucket = client.bucket(args.bucket)

    # setup basemap projection
    print("WORKER [%d]: setting up basemap projection" % i)
    m = setup_basemap_proj(lats, lons)
    mx, my = m(lons, lats)

    # rendering, saving, uploading loop
    while True:
        # get next job or it none, quit
        tmp = jobs.get()
        if tmp == None:
            return

        data, caxis, cmap, riak_key, fname = tmp

        print("WORKER [%d]: rendering job %d ..." % (i, done+1)) 

        # perform the render
        render_to_png(fig, m, data, lats, lons, mx, my, counties, caxis, cmap, fname)

        # run convert to generate jpeg
        jpeg_name = fname.replace(".png", ".jpeg")
        os.system("convert %s -resize 700x437 -quality 75 %s" % (fname, jpeg_name))

        # read in the file just generated
        with open(jpeg_name, 'rb') as f:
            fig_data = f.read()

        print("WORKER [%d]: uploading job %d ... " % (i, done+1))

        # store it in RIAK
        new_fig = bucket.new(riak_key,
                             encoded_data = fig_data,
                             content_type = 'image/jpeg')
        new_fig.store()

        done += 1
        print("WORKER [%d]: job %d uploaded. " % (i, done))


    print("WORKER [%d]: queue emptied, exiting." % i)


def render_to_png(fig, m, data, lats, lons, mx, my, counties, caxis, cmap, fname):
    # prep figure for reuse
    fig.clf()
    ax = plt.subplot(111)

    # retrieve geographical limits
    x1, x2 = np.amin(lons), np.amax(lons)
    y1, y2 = np.amin(lats), np.amax(lats)

    # paint the image
    m.pcolormesh(mx, my, data, shading='gouraud', cmap = cmap)

    # draw parallels/meridians
    m.drawparallels(np.arange(int(y1), int(y2)+1, 2.), labels=[1,0,0,0], color='black',
                    dashes=[1,0], labelstyle='+/-', linewidth=0.4)
    m.drawmeridians(np.arange(int(x1), int(x2)+1 ,2.), labels=[0,0,0,1], color='black',
                    dashes=[1,0], labelstyle='+/-', linewidth=0.2)

    # draw all the counties
    for county in counties:
        ax.add_collection(county)

    # add title & colorbar
    plt.clim(caxis)
    plt.colorbar(shrink = 0.9)

    fig.savefig(fname)
    fig.clf()



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
    parser.add_argument('-w', '--workers', metavar = 'W', type=int, dest='num_workers', default=10, help='number of workers to spawn')
    args = parser.parse_args()

    varlst = args.varlst or 'T2,RAIN,RH,FM1,FM10,FM100'
    varlst = string.split(varlst, ',')

    # load required variables
    load_lst = list(varlst)
    if 'RAIN' in load_lst:
        load_lst.remove('RAIN')
        load_lst.extend(['RAINC', 'RAINNC' ])

    if 'RH' in load_lst:
        load_lst.remove('RH')
        load_lst.extend(['T2', 'PSFC', 'Q2'])

    load_lst = list(set(load_lst))
    print("INFO: loading the following variables %s" % str(load_lst))

    print("INFO: loading wrf model output data ...")
    w = WRFModelData(args.wrfout_file, load_lst)
    ts = w['GMT']

    N = len(ts)

    if 'RH' in varlst:
        w.compute_relative_humidity()

    print("INFO: processing variables %s" % str(varlst))

    print("INFO: loading county shapes ...")
    lats, lons = w.get_lats(), w.get_lons()
    m = setup_basemap_proj(lats, lons)
    counties = load_colorado_shapes(m)

    print("INFO: found %d times in wrfout." % len(ts))

    plot_queue = Queue()

    # construct temporal parts of keys 
    ts_strings = []
    for t in range(N):
        ts_strings.append(ts[t].strftime("%Y-%m-%d_%H:%M:00"))

    # loop through variables requested
    for vname in varlst:
        print("INFO: enqueuing variable %s ..." % vname)
        rcfg = RenderConfig[vname]

        data = w[vname]
        if vname == 'T2':
            # convert to Fahrenheit
            data = (data - 273.15) * 9.0 / 5 + 32.0
        elif vname.startswith('FM'):
            data = data * 100.0

        caxis = rcfg['caxis'] or (np.amin(data), np.amax(data))
        cmap = rcfg['cmap']
        for t in range(N):
            riak_key = vname + "_" + ts_strings[t]
            fname = os.path.join(args.output_dir, riak_key + ".png")
            plot_queue.put((data[t, :, :], caxis, cmap, riak_key, fname))

    # start the worker pool
    workers = []
    for i in range(args.num_workers): 
        # end-of-queue marker (one for each worker)
        plot_queue.put(None)

        # create a new worker and add it to the pool
        tmp = Process(target=postproc_worker, args=(lats, lons, args, counties, plot_queue, i))
        tmp.start()
        workers.append(tmp)

    for worker in workers:
        worker.join()

    print("INFO: uploading key set to server.")
    client = riak.RiakClient(host = args.riak_host,
                             pb_port = args.riak_port,
                             protocol = 'pbc')

    bucket = client.bucket(args.bucket)
    valid_ts = bucket.new("current_valid_ts",
                          data = string.join(ts_strings, "|"),
                          content_type = "text/plain")
    valid_ts.store()

    print("INFO: key set stored.")
               

    

