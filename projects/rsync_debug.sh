~/bin/rsync.sh

cd ~/projects
rsync -avz --include-from=$HOME/bin/rsync_include.txt --include="*config.options" --exclude "*" visionworkbench/ visionworkbench_debug
rsync -avz --include-from=$HOME/bin/rsync_include.txt --include="*config.options" --exclude "*" StereoPipeline/ StereoPipeline_debug

export BASE=_debug; echo BASE=$BASE

perl -pi -e "s#ENABLE_OPTIMIZE=\w+#ENABLE_OPTIMIZE=no#g" visionworkbench$BASE/config.options StereoPipeline$BASE/config.options


perl -pi -e "s#visionworkbench/build#visionworkbench$BASE/build#g" StereoPipeline$BASE/config.options


# cd visionworkbench$BASE
# ./autogen; ./configure; make -j 16
# cd ..

# ~/bin/sym_install.sh

# cd StereoPipeline$BASE
# ./autogen; ./configure; make -j 16
# cd ..
