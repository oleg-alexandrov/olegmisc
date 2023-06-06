#!/usr/bin/python

import sys, os, re
import matplotlib.pyplot as plt
import numpy as np
import rasterio

def get_clim(ar):
    try:
        clim = np.percentile(ar.compressed(),(2,98))
    except:
        clim = np.percentile(ar,(2,98))
    return clim

def plot_ar(im,ax,clim,cmap=None,label=None,cbar=True,alpha=1):
    if cmap:
        img = ax.imshow(im,cmap=cmap,clim=clim,alpha=alpha,interpolation='none')
    else:
        img = ax.imshow(im,clim=clim,alpha=alpha,interpolation='none')
    if cbar:
        #divider = make_axes_locatable(ax)
        #cax = divider.append_axes("right", size="5%", pad=0.05)
        #cax = divider.append_axes("right", size="5%", pad="2%")
        #cax = divider.append_axes("right", size="2%", pad="1%")
        cb = plt.colorbar(img, ax=ax)
        #ca.set_ylabel(label)
    ax.set_xticks([])
    ax.set_yticks([])

def get_nodata_value(ds):
    no_data = ds.nodatavals[0]
    if no_data == None:
        #this means no data is not set in tif tag, nead to cheat it from raster
        ndv = ds.read(1)[0,0]
    else:
        ndv = no_data
    return ndv

def get_masked_image(fn,b=1):
    ds = rasterio.open(fn)
    ar = ds.read(b)
    ndv = get_nodata_value(ds)
    ma_ar = np.ma.masked_less_equal(ar,ndv)
    return ma_ar

def plot_files(files, titles):

    num = len(files)
    fig, axes = plt.subplots(1, num, figsize = (12, 6))

    cmap_err = 'inferno'
    for it in range(num):
        img = get_masked_image(files[it])
        plot_ar(img, ax=axes[it], clim = get_clim(img), cbar = True, cmap = cmap_err)
        axes[it].title.set_text(titles[it])
        
    plt.show()

plot_files(sys.argv[1:], ['Orig IntersectionErr', 'Corrected IntersectionErr', 'DEM change abs diff'])




