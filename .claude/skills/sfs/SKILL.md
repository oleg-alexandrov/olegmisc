---
name: sfs
description: Shape-from-Shading (SfS) and the canonical batch mapprojection framework in ~/projects/sfs/. Load before running batch mapprojection across multiple images, setting up SfS illumination modeling, running parallel_sfs, or preparing tiles and orthoimages for photoclinometry.
---

# Shape-from-Shading (SfS) & Canonical Batch Mapprojection

This skill documents the Ames Stereo Pipeline Shape-from-Shading (SfS) photoclinometry toolset and the canonical batch mapprojection framework maintained in `~/projects/sfs/`.

## Do Not Reinvent the Wheel: Use the Standard Batch Mapproject Scripts

When preparing mapprojected images for SfS, bundle adjustment, GCP generation, or mosaic building across many images (from 10 to 1300+ images on large grids like 20k x 20k pixels):
- **NEVER** write ad-hoc mapprojection loops or custom submission scripts from scratch.
- **ALWAYS** use the robust, tested scripts in `~/projects/sfs/`:
  - `~/projects/sfs/batch_mapproject.sh`: Slices an image list into chunks (default 30 images per job) and submits concurrent PBS jobs to Pleiades/Athena.
  - `~/projects/sfs/mapproject_chunk.sh`: Worker script executed on each compute node to mapproject its assigned slice of images.

### Features of the Canonical Scripts
1. **Arbitrary Sensor Support**:
   - Seamlessly handles LRO NAC product IDs (`M...LE/RE`), Chandrayaan-2 OHRC (`ch2_ohr_...`), MRO CTX, or full/relative filesystem paths to `.cub` or `.tif` files.
2. **Flexible Camera Pairing**:
   - **Glob Mode**: Camera for image id `F` is resolved via `${baPrefix}*${F}*.json`.
   - **Paired List Mode** (optional argument `cameraList.txt`): Line $N$ of `cameraList.txt` matches line $N$ of `imageList.txt`, allowing cameras from arbitrary directories, subfolders, or differing naming schemes.
3. **Resolution Configuration (`--tr`)**:
   - Set `TR=<meters>` (e.g. `TR=0.5` or `TR=1.0`) to mapproject all images to a constant, identical ground resolution.
   - Set `TR_COL=<col_idx>` to extract per-image native resolution from that column of `imageList.txt`.
   - Unset defaults to legacy `--tr 1`.
4. **Target Extent (`--t_projwin`)**:
   - Set `PROJWIN="<xmin> <ymin> <xmax> <ymax>"` to constrain all mapprojections to a specific bounding box (e.g. a 3 km site or 20k x 20k regional tile).
   - When unset, each image is mapprojected across its full intersection with the DEM.
5. **Robust Error Handling**:
   - Catches out-of-range footprint exceptions non-fatally (e.g. images that do not intersect the bounding box or produce unphysical horizon ray projections).
   - Cleans up 0-byte partial GeoTIFFs and leftover `*_tif_tiles` scratch directories so failed runs leave no stale artifacts.
   - Disables core dumps (`ulimit -c 0`) to prevent disk exhaustion.
   - Automatically skips already-completed, non-empty GeoTIFFs (`-s "$map"`).

---

## Running Batch Mapprojection

### Invocation Syntax

```bash
# Basic usage with baPrefix glob
~/projects/sfs/batch_mapproject.sh dem.tif imageList.txt baPrefix mapDir currDir

# Paired list usage with explicit camera list
~/projects/sfs/batch_mapproject.sh dem.tif imageList.txt ignored mapDir currDir cameraList.txt
```

### Environment Variables for `batch_mapproject.sh`

Control job dispatch and mapprojection settings via environment variables:

| Variable | Default | Description |
| :--- | :--- | :--- |
| `TR` | *(unset)* | Fixed grid resolution in meters/pixel (e.g. `TR=0.5`) |
| `TR_COL` | *(unset)* | Column in `imageList.txt` with per-image native GSD |
| `PROJWIN` | *(unset)* | Target extent `"xmin ymin xmax ymax"` |
| `CHUNK_SIZE` | `30` | Number of images per PBS worker job |
| `PROCESSES` | `10` | Worker processes per node inside `mapproject` |
| `THREADS` | *(ASP default)* | Threads per process inside `mapproject` |
| `TILE_SIZE` | `2048` | Tile dimension for parallel mapprojection |
| `NO_MOSAIC` | *(unset)* | Set to 1 to skip the per-chunk `dem_mosaic --max` |
| `EXTRA_OPTS`| *(unset)* | Additional flags passed directly to `mapproject` |
| `MODEL` | `bro_ele` | PBS node architecture (`bro_ele`, `tur_ath`, etc.) |
| `NCPUS` | `28` | Number of CPUs requested per PBS node |
| `WALLTIME` | `6:00:00` | PBS walltime limit per job |
| `IMG_DIR` | `img` | Directory prefix for relative image paths |
| `IMG_EXT` | `.cal.echo.cub` | File extension for image paths |

### Example: Mapprojecting OHRC and LRO NAC on a 3 km Grid

```bash
cd /nobackupp19/oalexan1/projects/my_project

export TR=0.5
export PROJWIN="-12500 -10500 -9500 -13500"
export NO_MOSAIC=1
export CHUNK_SIZE=30
export WALLTIME="04:00:00"

~/projects/sfs/batch_mapproject.sh \
  ref_dem.tif \
  lists/images.txt \
  ignored \
  maps_3km \
  $(pwd) \
  lists/cameras.txt
```

---

## Canonical SfS Toolkit Reference (`~/projects/sfs/`)

The `~/projects/sfs/` repository contains the core pipeline scripts developed for photoclinometry on Pleiades and Athena:

* **`batch_mapproject.sh`**: Chunked PBS job orchestrator for multi-node parallel mapprojection.
* **`mapproject_chunk.sh`**: Robust per-node worker script with error recovery and tile cleanup.
* **`parallel_sfs.sh`**: Distributed Shape-from-Shading runner across multiple tiles and nodes.
* **`sfs_sim_align.sh`**: Measures pointing errors against simulated illumination and runs single-camera bundle adjustment before SfS.
* **`query_azimuth.sh`**: Fast extraction of camera solar azimuth and elevation via `sfs --query`.
* **`query_gsd.sh`**: Automatic querying of native ground sampling distance via `mapproject --query-projection`.
* **`blend_img_mosaic.sh` / `avg_mosaic.sh`**: Weighted-mean blending of mapprojected images with shadow suppression.
* **`bundle_adjust_dem_gcp.sh`**: Bundle adjustment constrained by DEM surface and ground control points.
