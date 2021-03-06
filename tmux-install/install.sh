#!/bin/bash

# Script for installing tmux and dependencies.
# tmux will be installed in /usr/local/lib by default.

# Prerequisites: - gcc
#                - curl

# define versions

tmux_version="2.7"
# leave empty for stable releases
#tmux_patch_version="a"
tmux_patch_version=""

# yum list installed | grep libevent
libevent_version="2.0.21"
# yum list installed | grep ncurses
ncurses_version="5.9"
#
xclip_version="0.12"

tmux_name="tmux-$tmux_version$tmux_patch_version"
libevent_name="libevent-$libevent_version-stable"
ncurses_name="ncurses-$ncurses_version"
xclip_name="xclip-$xclip_version"

tmux_src_file="$tmux_name.tar.gz"
libevent_src_file="$libevent_name.tar.gz"
ncurses_src_file="$ncurses_name.tar.gz"
xclip_src_file="${xclip_name}.tar.gz"
# set the installation directory

#target_dir="/usr/local"
target_dir="${HOME}/opt/tmux-${tmux_version}"
test -d $target_dir || mkdir -p $target_dir

#work_dir="/tmp"
work_dir="."

# download source files for tmux, libevent, and ncurses
# save them in work_dir
pushd $work_dir

test -e $tmux_src_file || curl -L https://github.com/tmux/tmux/releases/download/$tmux_version$tmux_patch_version/$tmux_src_file -o $tmux_src_file
test -e $libevent_src_file || curl -L https://github.com/downloads/libevent/libevent/$libevent_src_file -o $libevent_src_file
test -e $ncurses_src_file || curl -L https://ftp.gnu.org/pub/gnu/ncurses/$ncurses_src_file -o $ncurses_src_file
test -e $xclip_src_file || curl -L http://kent.dl.sourceforge.net/project/xclip/xclip/${xclip_version}/$xclip_src_file -o $xclip_src_file

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

# xclip installation
tar xvzf $xclip_src_file
cd $xclip_name
./configure --prefix=$target_dir 
make
make install
cd -

# tmux installation
tar xvzf $tmux_src_file
cd ${tmux_name}*/
cflags="-I$target_dir/include -I$target_dir/include/ncurses"
ldflags="-L$target_dir/lib -L$target_dir/include/ncurses -L$target_dir/include"
./configure CFLAGS="${cflags}" LDFLAGS="${ldflags}"
CPPFLAGS="${cflags}" LDFLAGS="-static ${ldflags}" make
cp tmux $target_dir/bin
cd -

version=`$target_dir/bin/tmux -V | cut -d ' ' -f 2`
if [ -z "$version" ]; then
  echo 
  echo "[error] failed to install tmux - check for errors in the above output"
  exit 1
else
  echo add $target_dir/bin to your PATH
fi

popd
