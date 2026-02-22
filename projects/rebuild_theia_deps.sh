# How to rebuild TheiaSfM (build_reconstruction, etc.) in the ASP deps tarballs
# for Mac ARM64 and Mac x86_64, and push updated tarballs to GitHub.
#
# Context: TheiaSfM lives in ~/projects/MultiView/TheiaSfM as a submodule.
# ASP deps tarballs are stored as GitHub releases on NeoGeographyToolkit/BinaryBuilder.
# The CI scripts in StereoPipeline/.github/workflows/ reference these tarballs by tag.
#
# Done in Feb 2026 to add --random_seed flag to build_reconstruction for
# reproducible SfM results on Mac.

# ============================================================================
# 0. Prerequisites
# ============================================================================

# Need gh CLI (installed in a conda env)
# ~/anaconda3/envs/gh/bin/gh

# Need cmake from asp_deps env
# ~/miniconda3/envs/asp_deps/bin/cmake

# Update TheiaSfM submodule to latest
cd ~/projects/MultiView
git submodule update --init --recursive
cd TheiaSfM && git fetch && git checkout master && git pull origin master && cd ..

# ============================================================================
# 1. Check current release tags
# ============================================================================

~/anaconda3/envs/gh/bin/gh release list -R NeoGeographyToolkit/BinaryBuilder

# As of Feb 2026:
# asp_deps_mac_arm64_v2  (Mac ARM64)
# asp_deps_mac_x64_v2    (Mac Intel)
# asp_deps_linux_v1      (Linux)

# ============================================================================
# 2. Build for Mac ARM64 (native)
# ============================================================================

# Download and extract ARM deps
cd ~
wget https://github.com/NeoGeographyToolkit/BinaryBuilder/releases/download/asp_deps_mac_arm64_v2/asp_deps.tar.gz
tar xzf asp_deps.tar.gz -C $HOME
# Creates ~/miniconda3/envs/asp_deps (ARM64) and ~/miniconda3/envs/python_isis9

# Prepare MultiView CMakeLists.txt: comment out everything except TheiaSfM.
# Keep: OpenMP, Threads, GFlags, GLog, Eigen, BLAS, Ceres, and the TheiaSfM
# add_subdirectory block at the bottom.
# Comment out: OpenGL, PNG, JPEG, TIFF, TBB, Boost, Jasper, OpenCV, PCL,
# and all add_subdirectory lines except TheiaSfM (mapmap, mve, rayint,
# texrecon, cgal_tools, voxblox).

# Create FindFLANN.cmake shim (FLANN libs exist but have no cmake config)
cat > ~/projects/MultiView/TheiaSfM/cmake/FindFLANN.cmake << 'FLANNEOF'
find_path(FLANN_INCLUDE_DIRS flann/flann.hpp
  HINTS ${MULTIVIEW_DEPS_DIR}/include)
find_library(FLANN_LIBRARIES NAMES flann_cpp
  HINTS ${MULTIVIEW_DEPS_DIR}/lib)
find_library(FLANN_LIBRARIES_STATIC NAMES flann_cpp_s
  HINTS ${MULTIVIEW_DEPS_DIR}/lib)
if(FLANN_INCLUDE_DIRS AND FLANN_LIBRARIES)
  set(FLANN_FOUND TRUE)
  message(STATUS "Found FLANN: ${FLANN_LIBRARIES}")
else()
  message(FATAL_ERROR "Cannot find FLANN")
endif()
FLANNEOF

# Configure and build
DEPS=~/miniconda3/envs/asp_deps
mkdir -p ~/projects/MultiView/build && cd ~/projects/MultiView/build
$DEPS/bin/cmake .. \
  -DCMAKE_BUILD_TYPE=Release \
  -DCMAKE_C_COMPILER=$DEPS/bin/clang \
  -DCMAKE_CXX_COMPILER=$DEPS/bin/clang++ \
  -DMULTIVIEW_DEPS_DIR=$DEPS \
  -DCMAKE_OSX_DEPLOYMENT_TARGET=10.13 \
  -DCMAKE_VERBOSE_MAKEFILE=ON \
  "-DCMAKE_CXX_FLAGS=-O3 -Wno-error" \
  "-DCMAKE_C_FLAGS=-O3 -Wno-error" \
  -DCMAKE_INSTALL_PREFIX=$DEPS

make -j8
make install

# Verify
file $DEPS/bin/build_reconstruction
# Should say: Mach-O 64-bit executable arm64
$DEPS/bin/build_reconstruction --help | grep random_seed
# Should show: -random_seed (Random seed for reproducible results...)

# Clean up MultiView
cd ~/projects/MultiView
git checkout CMakeLists.txt
rm -f TheiaSfM/cmake/FindFLANN.cmake

# ============================================================================
# 3. Cross-compile for Mac x86_64 (on ARM Mac)
# ============================================================================

# Download and extract x86 deps to a SEPARATE directory (not $HOME)
mkdir -p ~/projects/x86_deps && cd ~/projects/x86_deps
~/anaconda3/envs/gh/bin/gh release download asp_deps_mac_x64_v2 \
  -R NeoGeographyToolkit/BinaryBuilder -p "asp_deps.tar.gz"
tar xzf asp_deps.tar.gz
# Creates ~/projects/x86_deps/miniconda3/envs/asp_deps (x86_64)

# Verify it's x86_64
file ~/projects/x86_deps/miniconda3/envs/asp_deps/bin/build_reconstruction
# Should say: Mach-O 64-bit executable x86_64

# Same CMakeLists.txt and FindFLANN.cmake edits as ARM (see above).

# Key cross-compile trick:
# - Use the ARM conda clang (it can produce x86_64 code with -arch x86_64)
# - Set CMAKE_OSX_ARCHITECTURES=x86_64
# - Use -nostdlib++ to prevent linking the ARM libc++
# - Explicitly link x86_64 libc++ and libc++abi from the x86 deps env
#
# System clang does NOT work because CMake's try_compile tests (BLAS, pthreads)
# produce x86_64 binaries that can't run on the ARM host.

X86_DEPS=~/projects/x86_deps/miniconda3/envs/asp_deps
ARM_DEPS=~/miniconda3/envs/asp_deps  # for cmake and clang binaries

mkdir -p ~/projects/MultiView/build_x86 && cd ~/projects/MultiView/build_x86
$ARM_DEPS/bin/cmake ~/projects/MultiView \
  -DCMAKE_BUILD_TYPE=Release \
  -DCMAKE_C_COMPILER=$ARM_DEPS/bin/clang \
  -DCMAKE_CXX_COMPILER=$ARM_DEPS/bin/clang++ \
  -DCMAKE_OSX_ARCHITECTURES=x86_64 \
  -DMULTIVIEW_DEPS_DIR=$X86_DEPS \
  -DCMAKE_OSX_DEPLOYMENT_TARGET=10.13 \
  -DCMAKE_VERBOSE_MAKEFILE=ON \
  "-DCMAKE_CXX_FLAGS=-O3 -Wno-error -nostdlib++" \
  "-DCMAKE_C_FLAGS=-O3 -Wno-error" \
  "-DCMAKE_SHARED_LINKER_FLAGS=-L$X86_DEPS/lib -lc++ -lc++abi" \
  "-DCMAKE_EXE_LINKER_FLAGS=-L$X86_DEPS/lib -lc++ -lc++abi" \
  -DCMAKE_INSTALL_PREFIX=$X86_DEPS

make -j8
make install

# Verify (can't run it on ARM, but check the architecture)
file $X86_DEPS/bin/build_reconstruction
# Should say: Mach-O 64-bit executable x86_64

# Clean up MultiView
cd ~/projects/MultiView
git checkout CMakeLists.txt
rm -f TheiaSfM/cmake/FindFLANN.cmake

# ============================================================================
# 4. Re-tar and push updated deps
# ============================================================================

# ARM64: re-tar from home dir (tarball contains miniconda3/envs/)
cd $HOME
tar czf /tmp/asp_deps.tar.gz miniconda3/envs
# Bump version: v2 -> v3 (increment from current)
TAG=asp_deps_mac_arm64_v3
~/anaconda3/envs/gh/bin/gh release create $TAG /tmp/asp_deps.tar.gz \
  -R NeoGeographyToolkit/BinaryBuilder \
  --title "$TAG" \
  --notes "Mac ARM64 deps with updated TheiaSfM"

# x86_64: re-tar from the x86_deps dir
cd ~/projects/x86_deps
tar czf /tmp/asp_deps.tar.gz miniconda3/envs
TAG=asp_deps_mac_x64_v3
~/anaconda3/envs/gh/bin/gh release create $TAG /tmp/asp_deps.tar.gz \
  -R NeoGeographyToolkit/BinaryBuilder \
  --title "$TAG" \
  --notes "Mac x86_64 deps with updated TheiaSfM"

# Verify
~/anaconda3/envs/gh/bin/gh release list -R NeoGeographyToolkit/BinaryBuilder

# IMPORTANT: The tarball asset MUST be named asp_deps.tar.gz because
# build_test.sh downloads it by that name. Use /tmp/asp_deps.tar.gz as
# the source file to ensure the correct name.

# ============================================================================
# 5. Update StereoPipeline CI scripts
# ============================================================================

# Files to update (change tag versions to match what you just pushed):
#   .github/workflows/build_test.sh  — lines ~37 and ~41
#   .github/workflows/build_helper.sh — lines ~37-39

cd ~/projects/StereoPipeline

# Example: updating x86 tag from v2 to v3
# sed -i '' 's/asp_deps_mac_x64_v2/asp_deps_mac_x64_v3/g' \
#   .github/workflows/build_test.sh .github/workflows/build_helper.sh

# Example: updating arm64 tag from v2 to v3
# sed -i '' 's/asp_deps_mac_arm64_v2/asp_deps_mac_arm64_v3/g' \
#   .github/workflows/build_test.sh .github/workflows/build_helper.sh

git add .github/workflows/build_test.sh .github/workflows/build_helper.sh
git commit -m "Update Mac deps tags to v3 (rebuilt TheiaSfM)"
git push origin master
git push god master

# ============================================================================
# 6. Cleanup (all of this is re-downloadable from the cloud)
# ============================================================================

rm -rf ~/projects/x86_deps
rm -rf ~/projects/MultiView/build ~/projects/MultiView/build_x86
rm -f /tmp/asp_deps.tar.gz
# Keep ~/miniconda3/envs/asp_deps and ~/miniconda3/envs/python_isis9

# ============================================================================
# Notes and gotchas
# ============================================================================

# - BUILD_VIEW_RECONSTRUCTION is OFF by default in TheiaSfM. No need to
#   disable it explicitly. Only TheiaSfM stuff gets installed: binaries
#   (build_reconstruction, export_to_nvm_file, export_reconstruction_to_vw,
#   export_matches_file_to_vw) and libraries (libtheia, libakaze, etc.)

# - The x86 deps env has x86_64 libc++ and libgomp. The ARM conda clang
#   can compile for x86_64 (-arch x86_64 via CMAKE_OSX_ARCHITECTURES) but
#   its own libc++ is ARM. So we use -nostdlib++ to suppress the ARM one
#   and explicitly link the x86_64 one with -L and -lc++ -lc++abi.

# - System clang (/usr/bin/clang) fails for cross-compile because CMake's
#   try_compile tests (FindBLAS, FindThreads) produce x86_64 binaries that
#   can't execute on the ARM host. The conda clang works because it finds
#   deps via CMAKE_PREFIX_PATH without needing to run test binaries.

# - The gh release create command uses the actual filename of the uploaded
#   file as the asset name. So copy your tarball to /tmp/asp_deps.tar.gz
#   before uploading to ensure the asset is named asp_deps.tar.gz.

# - Always increment the version tag (v1 -> v2 -> v3) so old tarballs
#   are preserved as fallback.

# - After pushing the tarball, you MUST push the tag update to
#   StereoPipeline (both origin and god remotes) so the overnight CI
#   picks up the new deps.
