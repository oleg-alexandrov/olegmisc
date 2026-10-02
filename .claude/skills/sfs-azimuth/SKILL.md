---
name: sfs-azimuth
description: >-
  Solar azimuth analysis and polar rose plotting for Shape-from-Shading (SfS) and camera co-registration:
  querying sun geometry with sfs --query, tiny DEM sizing rules (never over 1000x1000), large CSM pose sample
  loading overhead, canonical batch querying with sfs_query.sh, and constructing multi-dataset polar azimuth plots.
---

# Solar Azimuth Analysis & Polar Plots for Shape-from-Shading (SfS)

This skill documents solar azimuth geometry in Shape-from-Shading (SfS) photoclinometry and camera co-registration, the mechanics and gotchas of Ames Stereo Pipeline's `sfs --query`, and how to construct multi-dataset polar rose plots.

## 1. Why Solar Azimuth Matters for SfS & Registration

At lunar and planetary polar latitudes (e.g. latitude -83° to -90°S), the Sun is permanently low on the horizon (solar elevation typically 1° to 5°). Under grazing illumination, scene appearance is completely dominated by topographic shadows:
- **Shadows Orient Along Sun Azimuth**: Surface shadows point directly away from the Sun (shadow azimuth = sun azimuth + 180°). While elevation controls shadow length, azimuth dictates shadow direction.
- **Registration Requires Matched Shadows**: Interest-point matching and cross-correlation between different orbits or instruments (e.g. LRO NAC vs Chandrayaan-2 OHRC, or multi-temporal NAC pairs) only succeed when images share close solar azimuths. Blended mosaics wash out shadows and degrade feature tracking.
- **SfS Multi-Illumination Diversity**: Photoclinometry (SfS) reconstructs surface slopes from image brightness variations. To resolve topography in all orthogonal directions without degeneracy or smearing, SfS requires images spanning multiple distinct azimuths across the allowable celestial arc.

## 2. Astronomical Illumination Constraints at Polar Latitudes

Due to the small obliquity of the Moon (axial tilt of 1.54°), the sub-solar latitude never departs beyond ±1.54° of the equator:
- At southern polar sites (e.g. -83.75°S), the Sun remains permanently in the northern equatorial sky.
- **Allowable Arc**: The Sun sweeps only through an azimuth range from roughly 60° (morning East) through 180° (noon North in local polar coordinates) to 270° (evening West) - an arc of ~210°.
- **Astronomically Forbidden Sector**: Azimuths between 300° and 60° (across true South) are physically impossible. The Sun never rises in the southern sky at this latitude. No spacecraft can ever observe illumination from this sector.

## 3. Querying Solar Geometry with ASP `sfs --query`

The canonical ASP tool to evaluate local solar azimuth and elevation is `sfs --query`:
```bash
sfs --threads 1 --query \
  -i <dem.tif> \
  --image-list <images.txt> \
  --camera-list <cameras.txt> \
  -o <out_prefix>
```

### Critical Gotcha 1: Tiny DEM Sizing Rule (Never Over 1000 x 1000 Pixels)
- `sfs --query` evaluates the solar azimuth and elevation at the center coordinates of the supplied DEM.
- **The DEM is only used to fix the ground location**: Ray intersection is evaluated at the DEM center point.
- **Overkill Trap**: Feeding a full-resolution regional DEM (e.g. 20,000 x 20,000 pixels, 1.6 GB) causes ASP to allocate massive internal image buffers and triggers an immediate out-of-memory kernel kill (exit code 137) on shared nodes.
- **Rule**: Always crop or supply a *tiny DEM* (e.g. 100 x 100 pixels, and **never over 1,000 x 1,000 pixels**). Crop with GDAL:
  ```bash
  gdal_translate -srcwin 9950 9950 100 100 ref_dem_large.tif ref_dem_small.tif
  ```

### Critical Gotcha 2: Large CSM Pose Sample Loading Overhead (>20k States)
- **Mechanism**: In ALE (`ale/base/type_sensor.py`), the default ephemeris reduction mode is `none`. Consequently, `num_samples = self.image_lines + 1`.
- For standard full-length LRO NAC images with 22,528 lines, the generated ISD JSON contains exactly **22,529 pose positions and 22,529 orientation quaternions** (one sample per scanline).
- When CSM initializes `USGS_ASTRO_LINE_SCANNER_SENSOR_MODEL`, constructing spline/Lagrange interpolators across 22,529 states takes **8 to 10 seconds of CPU time per camera**.
- This overhead occurs once per camera during model instantiation. For hundreds or thousands of images, serial execution takes hours. Mitigation: parallelize across cores using chunked lists.

## 4. The Canonical Batch Script: `~/projects/sfs/sfs_query.sh`

The reusable batch script `~/projects/sfs/sfs_query.sh` automates environment setup, memory protection, and optional multi-process execution:
```bash
# Usage:
~/projects/sfs/sfs_query.sh <dem.tif> <image_list.txt> <camera_list.txt> <out_dir> [num_processes]
```

### Architecture of `sfs_query.sh`:
1. Sets up the ISIS and ASP runtime environment:
   ```bash
   export ISISDATA=$HOME/projects/isis3data
   export ISISROOT=/swbuild/oalexan1/miniconda3/envs/isis10asp
   export ALESPICEROOT=$ISISDATA
   export PATH=$HOME/projects/BinaryBuilder/StereoPipeline/bin:$HOME/projects/BinaryBuilder/StereoPipeline/libexec:$ISISROOT/bin:$PATH
   export LD_LIBRARY_PATH=$ISISROOT/lib:$HOME/projects/BinaryBuilder/StereoPipeline/lib:$HOME/projects/BinaryBuilder/StereoPipeline/lib/csmplugins:$LD_LIBRARY_PATH
   umask 022
   ulimit -c 0
   ```
2. When `num_processes=1` (head node safe): executes `sfs --threads 1 --query` sequentially.
3. When `num_processes > 1` (compute node): splits image and camera lists into $N$ chunks and runs them concurrently in the background, combining outputs upon completion.
4. Parses outputs into `azimuth_table.txt`:
   `image_path raw_azimuth normalized_azimuth_0_to_360 elevation`

### Parsing Regex for Free-Form Logs (`parse_azimuth_logs.py`):
```python
import re
pattern = re.compile(r"Sun azimuth and elevation for:\s*(\S+)\s+are\s+([-\d\.]+)\s+and\s+([-\d\.]+)")
for line in fh:
  m = pattern.search(line)
  if m:
    img, raw_az, el = m.group(1), float(m.group(2)), float(m.group(3))
    norm_az = (raw_az % 360.0 + 360.0) % 360.0
```

## 5. Constructing Multi-Dataset Polar Azimuth Plots

To visualize coverage across datasets (e.g. baseline catalog vs newly ingested additional imagery):
- **Polar Frame**: 0° at North (top), clockwise direction (+East at 90°, South at 180°, West at 270°).
- **Concentric Radial Tiers**: Radius is an arbitrary visual separator, not physical data. Place baseline images on an inner circle ($r=1.0$) and additional images on an outer circle ($r=1.35$).
- **Styling**: Solid colored balls with dark edge rings; dashed guideline circles; gray shading across the astronomically forbidden southern sector (300° to 60°).

### Canonical Plotting Tool: `~/projects/sfs/plot_sfs_azimuth.py`
```bash
# Generate polar rose plot from sfs_query.sh output:
~/projects/sfs/plot_sfs_azimuth.py azimuth_table.txt -o solar_azimuth_polar.png

# Compare two datasets on dual concentric rings:
~/projects/sfs/plot_sfs_azimuth.py baseline_azimuth.txt --table2 additional_azimuth.txt \
  --label1 "Baseline Catalog" --label2 "Additional Imagery" \
  -o solar_azimuth_comparison.png
```

### Matplotlib Recipe:
```python
import matplotlib.pyplot as plt
import numpy as np

fig = plt.figure(figsize=(9, 9), dpi=300)
ax = fig.add_subplot(111, projection='polar')
ax.set_theta_zero_location('N')
ax.set_theta_direction(-1)

# Guidelines
theta = np.linspace(0, 2*np.pi, 500)
ax.plot(theta, [1.0]*len(theta), color='#b0bec5', linestyle='--', linewidth=0.8)
ax.plot(theta, [1.35]*len(theta), color='#b0bec5', linestyle='--', linewidth=0.8)

# Shaded forbidden sector (300 to 60 deg)
ax.fill_between(np.linspace(np.radians(300), np.radians(360), 50), 0, 1.55, color='#eceff1', alpha=0.4)
ax.fill_between(np.linspace(0, np.radians(60), 50), 0, 1.55, color='#eceff1', alpha=0.4)

# Inner tier: baseline catalog
ax.scatter(np.radians(moses_az), [1.0]*len(moses_az), c='#1f77b4', s=45, alpha=0.75,
           edgecolors='#0d47a1', linewidths=0.5, label='Baseline Catalog')

# Outer tier: additional images
ax.scatter(np.radians(add_az), [1.35]*len(add_az), c='#ff7f0e', s=55, alpha=0.85,
           edgecolors='#bf360c', linewidths=0.6, label='Additional Imagery')

ax.set_thetagrids(np.arange(0, 360, 30), labels=['0° (N)', '30°', '60°', '90° (E)', '120°', '150°',
                                                 '180° (S)', '210°', '240°', '270° (W)', '300°', '330°'])
ax.set_rmax(1.55)
ax.set_yticklabels([])
ax.set_rticks([])
ax.legend(loc='lower center', bbox_to_anchor=(0.5, -0.15), ncol=2)
plt.savefig('solar_azimuth_polar_plot.png', dpi=300, bbox_inches='tight')
```

## 6. Version Control Discipline: Do Not Commit PNGs to Git

Generated polar rose plots (`*.png`) and figures are visual inspection artifacts, NOT text metadata:
- **Never add `.png` files to the git repository**: Leave generated figures untracked on disk in the project directory.
- **Commit text metadata only**: In `lists/`, only the parsed azimuth table (`lists/azimuth_table.txt`) and matching image/camera text lists (`lists/*.txt`) belong in git.

