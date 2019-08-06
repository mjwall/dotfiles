#!/bin/bash

# Script for installing tmux and dependencies.
# tmux will be installed in /usr/local/lib by default.

# Prerequisites: - gcc
#                - curl

# define versions

tmux_version="2.9"
tmux_patch_version="a" # leave empty for stable releases

# yum list installed | grep libevent
libevent_version="2.0.21"
# yum list installed | grep ncurses
ncurses_version="5.9"

tmux_name="tmux-$tmux_version$tmux_patch_version"
libevent_name="libevent-$libevent_version-stable"
ncurses_name="ncurses-$ncurses_version"

tmux_src_file="$tmux_name.tar.gz"
libevent_src_file="$libevent_name.tar.gz"
ncurses_src_file="$ncurses_name.tar.gz"
# set the installation directory

target_dir="./tmux_install"
target_dir=$(realpath $target_dir)
test -d $target_dir || mkdir -p $target_dir
#target_dir="/usr/local"

#work_dir="/tmp"
work_dir="."

# download source files for tmux, libevent, and ncurses
# save them in work_dir
pushd $work_dir

test -e $tmux_src_file || curl -L https://github.com/tmux/tmux/releases/download/$tmux_version$tmux_patch_version/$tmux_src_file -o $tmux_src_file
test -e $libevent_src_file || curl -L https://github.com/downloads/libevent/libevent/$libevent_src_file -o $libevent_src_file
test -e $ncurses_src_file || curl -L https://ftp.gnu.org/pub/gnu/ncurses/$ncurses_src_file -o $ncurses_src_file
# extract files, configure, and compile

# libevent installation
tar xvzf $libevent_src_file
cd $libevent_name
./configure --prefix=$target_dir --disable-shared
make
make install
cd -

# ncurses installation
tar xvzf $ncurses_src_file
cd $ncurses_name
./configure --prefix=$target_dir
make
make install
cd -

# tmux installation
tar xvzf $tmux_src_file
cd ${tmux_name}*/
./configure CFLAGS="-I$target_dir/include -I$target_dir/include/ncurses" LDFLAGS="-L$target_dir/lib -L$target_dir/include/ncurses -L$target_dir/include"
CPPFLAGS="-I$target_dir/include -I$target_dir/include/ncurses" LDFLAGS="-static -L$target_dir/include -L$target_dir/include/ncurses -L$target_dir/lib"
make
cp tmux $target_dir/bin
cd -

version=`$target_dir/bin/tmux -V | cut -d ' ' -f 2`
if [ -z "$version" ]; then
  echo 
  echo "[error] failed to install tmux - check for errors in the above output"
  exit 1
fi

popd
