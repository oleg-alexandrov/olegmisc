---
name: docker-asp-build
description: >-
  Build Ames Stereo Pipeline (ASP) and its conda dependency feedstocks for Linux ARM (linux-aarch64) using Docker on the Apple Silicon Mac mini. Covers container lifecycle (asp_arm), native aarch64 execution, cross-arch cache isolation, package build workflow, uploading to Anaconda, and managing Docker host disk footprint.
---

# Docker Linux ARM Build System for ASP

This skill documents how to build and package Ames Stereo Pipeline (ASP) and its conda dependency feedstocks for Linux ARM (linux-aarch64) on the Apple Silicon Mac mini using Docker.

## Why Docker on Apple Silicon Mac

Apple Silicon (M-series) Macs run `linux/arm64` Docker containers natively without QEMU or Rosetta emulation. This allows compiling Linux ARM64 packages at near bare-metal hardware speeds locally without requiring remote ARM cloud instances.

Canonical history and coordination notes:
- `~/projects/env_update_06_2026_linux_arm.sh`: Linux ARM build log and package recipes.
- `~/projects/env_update_06_2026_coordination.sh`: Cross-platform build coordination across osx-arm64, osx-64, linux-64, and linux-aarch64.

## Container Architecture and Lifecycle

- Container Name: `asp_arm`
- Base Image: `condaforge/miniforge3:latest`
- Platform: `linux/arm64`
- Host Mount: `-v ~/projects:/projects`

### 1. Spawning the Container (if not running)
```bash
docker run -d --name asp_arm --platform linux/arm64 \
  -v ~/projects:/projects condaforge/miniforge3:latest sleep infinity
```

Install packaging tools into the `base` environment:
```bash
docker exec asp_arm mamba install -n base -y conda-build anaconda-client rattler-build conda-smithy
```

### 2. Checking Status
```bash
docker ps -f name=asp_arm
```

## Critical Safety Invariants

### 1. Host Mount Safety
`/projects` inside the container is a direct read-write bind mount to `/Users/oalexan1/projects` on the host:
- NEVER run recursive deletions (such as `rm -rf /projects/*`) from inside Docker.
- Output artifacts should be placed in designated channel folders like `/projects/asp_conda_channel`.

### 2. Cross-Architecture Package Cache Isolation
On the Mac mini, four distinct targets are built (osx-arm64, osx-64, linux-64, and linux-aarch64):
- NEVER bind-mount the host conda installation or host package cache into Docker.
- Keep the container's package cache strictly isolated in `/opt/conda/pkgs` or a temporary path (such as `CONDA_PKGS_DIRS=/tmp/pkgs_linuxaarch64`).
- Sharing package cache paths across architectures causes wrong-architecture dylibs/so libraries to be linked silently.

## Conda Environment Layout Inside Docker

To prevent disk bloat, keep only two environments inside `asp_arm`:
1. `base` (`/opt/conda`): Miniforge system holding `conda-build`, `mamba`, and packaging utilities.
2. `asp_deps` (`/opt/conda/envs/asp_deps`): The full Linux ARM dependency environment (Qt, GDAL, USGS CSM, Ceres, Boost, etc.).

Remove temporary test environments (`asp_test`, `pytest`, `python_isis10`) when testing finishes:
```bash
docker exec asp_arm /opt/conda/bin/conda env remove -n asp_test -y
```

## Package Build and Upload Workflow

### 1. Building a Conda Package
Drive the container using `docker exec`:
```bash
docker exec asp_arm bash -lc '
  cd /projects/<feedstock-dir>
  conda build --output-folder /projects/asp_conda_channel recipe
'
```
Built `.conda` or `.tar.bz2` packages will land directly in `/Users/oalexan1/projects/asp_conda_channel/linux-aarch64/` on the host.

### 2. Uploading to Anaconda Cloud
Always upload from the **host** (where the `nasa-ames-stereo-pipeline` credentials and token are already configured in `~/anaconda3`):
```bash
~/anaconda3/bin/anaconda upload /Users/oalexan1/projects/asp_conda_channel/linux-aarch64/<pkg>.conda
```
Do not authenticate or upload from inside the container.

## Managing Docker Host Disk Footprint

### How macOS Docker Storage Works
Docker Desktop for Mac stores container layers and writable volumes in a single virtual disk image:
`/Users/oalexan1/Library/Containers/com.docker.docker/Data/vms/0/data/Docker.raw`

Because macOS APFS manages `Docker.raw` as a sparse file, deleting files inside a container with `rm -rf` does not automatically reduce the file size on macOS immediately.

### Procedure to Shrink Docker Footprint

1. Clean temporary files and caches inside the container:
```bash
docker exec asp_arm rm -rf /tmp/pkgs_linuxaarch64 /tmp/tmp* /root/dryrun
docker exec asp_arm /opt/conda/bin/conda clean --all -y
```

2. Reclaim host disk space:
- If `Docker.raw` does not shrink automatically:
  - Open Docker Desktop -> Settings -> Resources -> Virtual disk limit.
  - Or restart Docker Desktop (Docker menu -> Restart Docker Desktop), which triggers APFS TRIM on the sparse disk.
  - Run `docker builder prune -a` to drop any dangling build cache.

3. Full Reset (if container is no longer needed):
```bash
docker rm -f asp_arm
docker system prune -a --volumes
```
Then use Docker Desktop's *Clean / Purge data* button to reset the disk image back to its minimal size.
