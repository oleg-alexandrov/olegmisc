#!/usr/bin/python

import sys, os, re
import matplotlib.pyplot as plt
import csv

import numpy as np
import os, sys, glob, shutil
from pyproj import Proj, transform, Transformer
from scipy.spatial.transform import Rotation as R

def produce_m(lon,lat,m_meridian_offset=0):
    """
    Produce M matrix which facilitates conversion from Lon-lat (NED) to ECEF coordinates
    From https://github.com/visionworkbench/visionworkbench/blob/master/src/vw/Cartography/Datum.cc#L249
    This is known as direction cosie matrix
    
    Parameters
    ------------
    lon: numeric
        longitude of spacecraft
    lat: numeric
        latitude of spacecraft
    m_meridian_offset: numeric
        set to zero
    Returns
    -----------
    R: np.array
        3 x 3 rotation matrix representing the m-matrix aka direction cosine matrix
    """
    if lat < -90:
        lat = -90
    if lat > 90:
        lat = 90
    
    rlon = (lon + m_meridian_offset) * (np.pi/180)
    rlat = lat * (np.pi/180)
    slat = np.sin(rlat)
    clat = np.cos(rlat)
    slon = np.sin(rlon)
    clon = np.cos(rlon)
    
    R = np.ones((3,3),dtype=float)
    R[0,0] = -slat*clon
    R[1,0] = -slat*slon
    R[2,0] = clat
    R[0,1] = -slon
    R[1,1] = clon
    R[2,1] = 0.0
    R[0,2] = -clon*clat
    R[1,2] = -slon*clat
    R[2,2] = -slat
    return R

def convert_ecef2NED(asp_rotation,lon,lat):
    """
    convert rotation matrices from ECEF to North-East-Down convention
    Parameters
    -------------
    asp_rotation: np.array
        3 x 3 rotation matrix from ASP
    lon: numeric
        longitude for computing m matrix
    lat: numeric
        latitude for computing m matrix
    
    Returns
    --------------
    r_ned: np.array
        3 x 3 NED rotation matrix 
    """
    m = produce_m(lon,lat)
    r_ned = np.matmul(np.linalg.inv(m),asp_rotation)
    #r_ned = np.matmul(np.transpose(m),asp_rotation)
    #r_ned = np.matmul(m,asp_rotation)
    return r_ned

ecef_proj = 'EPSG:4978'
geo_proj = 'EPSG:4326'
ecef2wgs = Transformer.from_crs(ecef_proj, geo_proj)

def calc_llh(ecef, cam):
    [x, y, z] = ecef
    lat_lon_h = ecef2wgs.transform(x, y, z)

    return lat_lon_h 

def read_tsai_dict(tsai):
    """
    read tsai frame model from asp and return a python dictionary containing the parameters
    See ASP's frame camera implementation here: https://stereopipeline.readthedocs.io/en/latest/pinholemodels.html
    Parameters
    ----------
    tsai: str
        path to ASP frame camera model
    Returns
    ----------
    output: dictionary
        dictionary containing camera model parameters
    #TODO: support distortion model
    """
    camera = os.path.basename(tsai)
    with open(tsai, 'r') as f:
        content = f.readlines()
    content = [x.strip() for x in content]
    fu = np.float64(content[2].split(' = ', 4)[1]) # focal length in x
    fv = np.float64(content[3].split(' = ', 4)[1]) # focal length in y
    cu = np.float64(content[4].split(' = ', 4)[1]) # optical center in x
    cv = np.float64(content[5].split(' = ', 4)[1]) # optical center in y
    cam = content[9].split(' = ', 10)[1].split(' ')
    cam_cen = [np.float64(x) for x in cam] # camera center coordinates in ECEF
    rot = content[10].split(' = ', 10)[1].split(' ')
    rot_mat = [np.float64(x) for x in rot] # rotation matrix for camera to world coordinates transformation
    pitch = np.float64(content[11].split(' = ', 10)[1]) # pixel pitch
    
    ecef_proj = 'EPSG:4978'
    geo_proj = 'EPSG:4326'
    ecef2wgs = Transformer.from_crs(ecef_proj, geo_proj)
    cam_ctr_lat_lon_h = ecef2wgs.transform(cam_cen[0], cam_cen[1], cam_cen[2]) # this returns lat, lon and height
    # cam_ctr_lat_lon_h = geolib.ecef2ll(cam_cen[0], cam_cen[1], cam_cen[2]) # camera center coordinates in geographic coordinates
    tsai_dict = {'camera':camera, 'focal_length':(fu, fv), 'optical_center':(cu, cv), 'cam_cen_ecef':cam_cen, 'cam_cen_wgs':cam_ctr_lat_lon_h, 'rotation_matrix':rot_mat, 'pitch':pitch}
    return tsai_dict

def ned_rotation_from_tsai(tsai_fn):
    #coordinate conversion step
    from pyproj import Transformer
    ecef_proj = 'EPSG:4978'
    geo_proj = 'EPSG:4326'
    ecef2wgs = Transformer.from_crs(ecef_proj,geo_proj)
    
    # read tsai files
    asp_dict = read_tsai_dict(tsai_fn)
    
    # get camera position
    cam_cen = asp_dict['cam_cen_ecef']
    lat,lon,h = ecef2wgs.transform(*cam_cen)
    #print(lat,lon)
    # get camera rotation angle
    rot_mat = np.reshape(asp_dict['rotation_matrix'],(3,3))
    
    #rotate about z axis by 90 degrees
    #https://math.stackexchange.com/questions/651413/given-the-degrees-to-rotate-around-axis-how-do-you-come-up-with-rotation-matrix
    rot_z = np.zeros((3,3),float)
    angle = np.pi/2
    rot_z[0,0] = np.cos(angle) 
    rot_z[0,1] = -1 * np.sin(angle)
    rot_z[1,0] = np.sin(angle)
    rot_z[1,1] = np.cos(angle)
    rot_z[2,2] = 1
    
    #return np.matmul(rot_z,convert_ecef2NED(rot_mat,lon,lat))
    return R.from_matrix(np.matmul(rot_z,np.linalg.inv(convert_ecef2NED(rot_mat,lon,lat)))).as_euler('ZYX',degrees=True)

def cam_lat_lon_h_from_tsai(tsai_fn):

    #coordinate conversion step
    from pyproj import Transformer
    ecef_proj = 'EPSG:4978'
    geo_proj = 'EPSG:4326'
    ecef2wgs = Transformer.from_crs(ecef_proj,geo_proj)
    
    # read tsai files
    asp_dict = read_tsai_dict(tsai_fn)
    
    # get camera position
    cam_ctr_lat_lon_h = asp_dict['cam_cen_wgs']
    return cam_ctr_lat_lon_h
    
def poly_fit(X, Y):
    """
    Fit a polynomial of degree 1 and return the fitted Y values.
    """
    fit = np.poly1d(np.polyfit(X, Y, 1))
    return fit(X)

def readCamCtr(fileName):
    camCtrs = {}
    with open(fileName) as file_obj:
        print("Opened: ", fileName)
        reader_obj = csv.reader(file_obj, delimiter = ' ')
        for row in reader_obj:
            cam = row[0]
            x = float(row[1])
            y = float(row[2])
            z = float(row[3])
            camCtrs[cam] = [x, y, z]
            
    return camCtrs

# Main function

if len(sys.argv) < 4:
    print("Usage: " + argv.sys[0] + " Num Types camPrefix")
    sys.exit(1)

Num   = int(sys.argv[1]) # How many to plot
Types = list(sys.argv[2]) # camera types, can be 'n', 'fna', etc.

# Assume cameras are named ${origPrefix}n1352.tsai and same for optPrefix
origPrefix = sys.argv[3]
optPrefix  = sys.argv[4]

origReport = sys.argv[5]
optReport  = sys.argv[6]

origLabel = sys.argv[7]
optLabel = sys.argv[8]
print("Camera types are: ", Types)
print("opt prefix ", optPrefix)

uluru = True
subtractLineFit = False # if to fit a line then subtract it

print("For Uluru: ", uluru)
print("Subtract line fit: ", subtractLineFit)

print("types is ", Types)
print("valsx ", origPrefix, optPrefix)

origCamCtrs = readCamCtr(origReport)
optCamCtrs = readCamCtr(optReport)

f, ax = plt.subplots(3, 3, sharex=True, sharey = False, figsize = (12, 9))

count = -1
for s in Types:

    print("Loading: " + s)
    count += 1

    if uluru:
        # Based on opt cameras find the original cameras. That because
        # maybe we optimized only a subset
        orig_cams = []
        opt_cams = sorted(glob.glob(optPrefix + s + '*.tsai'))
        for c in opt_cams:
            suff = c[-10:] # extract a1341.tsai
            orig_cams.append(origPrefix + suff)
    else:
        # Grand mesa
        orig_cams = []
        opt_cams = sorted(glob.glob(optPrefix + '/run-' + s + '*.tsai'))
        for c in opt_cams:
            d = c[-10:]
            e = c[-10:-9]
            f = e + '/' + d
            orig_cams.append(f)
            #print("c, f", c, f)

        print("number of cameras for view " + s + ': ' + str(len(orig_cams)))
        
    if len(orig_cams) != len(opt_cams):
        print("Number of original and opt cameras must be the same")
        sys.exit(1) 

    currNum = Num
    currNum = min(len(orig_cams), currNum)
    orig_cams = orig_cams[0:currNum]
    opt_cams = opt_cams[0:currNum]
    
    # Get ctrs, then convert to NED 
    orig_ctr_lat_lon_h = np.array([calc_llh(origCamCtrs[cam], cam) for cam in orig_cams])
    opt_ctr_lat_lon_h  = np.array([calc_llh(optCamCtrs[cam], cam)  for cam in orig_cams]) # use orig

    orig_h   = [r[2] for r in orig_ctr_lat_lon_h]
    orig_lon = [r[1] for r in orig_ctr_lat_lon_h]
    orig_lat = [r[0] for r in orig_ctr_lat_lon_h]

    opt_h  = [r[2] for r in opt_ctr_lat_lon_h]
    opt_lon  = [r[1] for r in opt_ctr_lat_lon_h]
    opt_lat  = [r[0] for r in opt_ctr_lat_lon_h]

    residualTag = ''
    if subtractLineFit:
        fit_lat = np.array(orig_lat) # poly_fit(np.array(range(len(orig_lat))), orig_lat)
        fit_lon = np.array(orig_lon) # poly_fit(np.array(range(len(orig_lon))), orig_lon)
        fit_h   = np.array(orig_h) # poly_fit(np.array(range(len(orig_h))), orig_h)

        orig_lat = orig_lat - fit_lat
        orig_lon = orig_lon - fit_lon
        orig_h = orig_h - fit_h
        
        opt_lat = opt_lat - fit_lat
        opt_lon = opt_lon - fit_lon
        opt_h = opt_h - fit_h

        residualTag = ' residual'
        
    # Plot residuals after subtracting a linear fit
    ax[count][0].plot(np.arange(len(orig_lat)), orig_lat, label=origLabel, color = 'b')
    ax[count][0].plot(np.arange(len(opt_lat)), opt_lat, label=optLabel, color = 'r')

    ax[count][1].plot(np.arange(len(orig_lon)), orig_lon, label=origLabel, color = 'b')
    ax[count][1].plot(np.arange(len(opt_lon)), opt_lon, label=optLabel, color = 'r')

    ax[count][2].plot(np.arange(len(orig_h)), orig_h, label=origLabel, color = 'b')
    ax[count][2].plot(np.arange(len(opt_h)), opt_h, label=optLabel, color = 'r')

    if s == 'a':
        t = 'aft'
    if s == 'n':
        t = 'nadir'
    if s == 'f':
        t = 'fwd'

    ax[count][0].set_title(t + ' lat'  + residualTag)
    ax[count][1].set_title(t + ' lon'  + residualTag)
    ax[count][2].set_title(t + ' h ' + residualTag)

    ax[count][0].set_ylabel('Degrees')
    ax[count][1].set_ylabel('Degees')
    ax[count][2].set_ylabel('Meters')

    ax[count][0].set_xlabel('Frame number')
    ax[count][1].set_xlabel('Frame number')
    ax[count][2].set_xlabel('Frame number')

    ax[count][0].legend()
    ax[count][1].legend()
    ax[count][2].legend()

plt.tight_layout()
plt.show()


