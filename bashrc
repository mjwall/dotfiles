# -*- mode: sh -*-

# Here is my bashrc.

prepend_path() {
  export PATH="$1:${PATH}"
}

set_if_exists() {
  local VARNAME=$1
  local VAR=$2
  if [ "${VAR}x" == "x" ]; then
    echo "${VARNAME} not set"
  elif [ ! -d "${VAR}" ]; then
    echo "${VARNAME} doesn't exist at ${VAR}"
  else
    export $VARNAME=${VAR}
    prepend_path "${VAR}/bin"
  fi
}

# put computer specific ENV VARS and aliases in here
# see bashrc-custom.sample for an example
if [ -e ~/.bashrc-custom ]; then
  source ~/.bashrc-custom
else
  echo "You need to copy the bashrc-custom.sample to ~/.bashrc-custom and customize"
fi

# Java
set_if_exists "JAVA_HOME" "${JAVA_HOME}"

# Maven
set_if_exists "M2_HOME" "${M2_HOME}"
if [ "${M2_HOME}x" != "x" ]; then
#  export JAVA_OPTS="-Duser.timezone=GMT -Djava.awt.headless=true"
#  #export MAVEN_OPTS="${JAVA_OPTS} -Xms512M -Xmx1024M -Xss1M -XX:MaxPermSize=128M"
#  export MAVEN_OPTS="${JAVA_OPTS} -Xms512M -Xmx1024M"
#  export MAVEN_OPTS_DEBUG="${MAVEN_OPTS} -Xdebug -Xnoagent -Djava.compiler=NONE -Xrunjdwp:transport=dt_socket,address=8781,server=y,suspend=n"
  export M2="$M2_HOME/bin"
fi

# Ant
#set_if_exists "ANT_HOME" "${ANT_HOME}"
#if [ "${ANT_HOME}x" != "x" ]; then
#   export ANT_OPTS="-Xms512M -Xmx2048M -Xss1M -XX:MaxPermSize=128M"
#fi

# if [ ! -z "$IVY2_REPO_DIR" ]; then
#   export ANT_OPTS="-Divy.default.ivy.user.dir=${IVY2_REPO_DIR} ${ANT_OPTS}"
# fi

# Leiningen, included in dotfiles/bin
# export LEIN_HOME=~/bin/lein-home

# # Scala, not sure what to do with this yet
# set_if_exists "SCALA_HOME" "${SCALA_HOME}"
# # sbt is in dotfiles/bin and should be setup

# editors
if [ ! -z ${EMACS_HOME} ]; then
    alias e="${EMACS_HOME}/bin/emacs -nw"
    #export GIT_EDITOR="${EMACS_HOME}/bin/emacs -nw"
    prepend_path ${EMACS_HOME}/bin
fi

_java_version() {
  echo $JAVA_VERSION
}

# git
GIT_COMPLETE=1
if [ "${GIT_COMPLETION_DIR}x" == "x" ]; then
  if [ `grep ^ID= /etc/os-release` == "ID=ubuntu" ]; then
    # https://askubuntu.com/questions/32507/how-do-i-get-a-list-of-installed-files-from-a-package
    # assuming apt install git
    if [ -e /etc/bash_completion.d/git-prompt ]; then
      source /etc/bash_completion.d/git-prompt
    fi
    if [ -e /usr/share/bash-completion/completions/git ]; then
      source /usr/share/bash-completion/completions/git
    fi
  elif [ `grep ^ID= /etc/os-release` == "ID=pop" ]; then
    if [ -e /etc/bash_completion.d/git-prompt ]; then
      source /etc/bash_completion.d/git-prompt
    fi
    if [ -e /usr/share/bash-completion/completions/git ]; then
      source /usr/share/bash-completion/completions/git
    fi
  else
    echo "No Git completion"
    GIT_COMPLETE=0
  fi
elif [ ! -e "${GIT_COMPLETION_DIR}/git-prompt.sh" ]; then
  echo "Git completion expecting git-prompt.sh"
else
  source "${GIT_COMPLETION_DIR}/git-completion.bash"
  source "${GIT_COMPLETION_DIR}/git-prompt.sh"
fi

# Read hostname from env if there, maybe from ~/.bashrc-custom
if [ "${PS1_HOST}x" == "x" ]; then
  PS1_HOST=$(hostname -s)
fi

if [ $GIT_COMPLETE -eq 1 ]; then
  export GIT_PS1_SHOWDIRTYSTATE=true
  # if __git_ps1 is slow
  #export GIT_PS1_SHOWDIRTYSTATE=
  #export GIT_PS1_SHOWUNTRACKEDFILES=
  export PS1='\[\e[1;32m\]\u@'"$PS1_HOST"'\[\e[1;34m\] \[\e[0m\]\[\e[1;36m\]\w\[\e[0m\]$(__git_ps1 " (%s)")\n\$> '
else
  export PS1='\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]\$ '
fi

# other completion scripts
source ~/.bash_completion.d/mvn-completion.bash
source ~/.bash_completion.d/svn-completion.bash

alias ..="cd .."
if [ $(uname) == "Darwin" ]; then
  # we don't need ._ files everywhere on a mac,
  # see http://www.commandlinefu.com/commands/view/5965/create-.tar-file-on-mac-os-x-leopard-snow-leopard-without-._-files
  alias tar="COPYFILE_DISABLE=true tar"
fi
alias ls='ls --color=auto'
alias ll='ls -la'
alias l.='ls -d .* --color=auto'

#history stuff
shopt -s histappend
PROMPT_COMMAND='history -a'
shopt -s cdspell
export HISTCONTROL="ignoredups"
export HISTIGNORE="&:ls:[bf]g:exit"
shopt -s cmdhist

# gpg agent
#export GPG_TTY='tty'
#if [ -f "${HOME}/.gpg-agent-info" ]; then
#  . "${HOME}/.gpg-agent-info"
#  export GPG_AGENT_INFO
#  # export SSH_AUTH_SOCK
#else
#  echo "Starting gpg-agent"
#fi

# ssh-agent
if [ -z "$SSH_AUTH_SOCK" ] ; then
  echo Starting ssh-agent
  eval `ssh-agent -s`
fi

# setup ~/bin
prepend_path "${HOME}/bin"

# Below is for reference only on the color codes
# original colors with ls -G or CLICOLORS = ExFxCxDxBxegedabagacad
#
#Hx directory
#Fx symbolic link
#Cx socket
#Dx pipe
#Bx executable
#eg block special
#ed character special
#ab executable with setuid bit set
#ag executable with setgid bit set
#ac directory writable to others, with sticky bit
#ad directory writable to others, without sticky bit
#
#a  black
#b  red
#c  green
#d  brown
#e  blue
#f  magenta
#c  cyan
#h  light grey
#A  block black, usually shows up as dark grey
#B  bold red
#C  bold green
#D  bold brown, usually shows up as yellow
#E  bold blue
#F  bold magenta
#G  bold cyan
#H  bold light grey; looks like bright white
#x  default foreground or background

# accumulo ulimit
#ulimit -S -n 65536

# output of dircolors command on an amazon linux box, trying to get consistent colors across Linux variants
LS_COLORS='rs=0:di=01;34:ln=01;36:mh=00:pi=40;33:so=01;35:do=01;35:bd=40;33;01:cd=40;33;01:or=40;31;01:su=37;41:sg=30;43:ca=30;41:tw=30;42:ow=34;42:st=37;44:ex=01;32:*.tar=01;31:*.tgz=01;31:*.arc=01;31:*.arj=01;31:*.taz=01;31:*.lha=01;31:*.lz4=01;31:*.lzh=01;31:*.lzma=01;31:*.tlz=01;31:*.txz=01;31:*.tzo=01;31:*.t7z=01;31:*.zip=01;31:*.z=01;31:*.Z=01;31:*.dz=01;31:*.gz=01;31:*.lrz=01;31:*.lz=01;31:*.lzo=01;31:*.xz=01;31:*.bz2=01;31:*.bz=01;31:*.tbz=01;31:*.tbz2=01;31:*.tz=01;31:*.deb=01;31:*.rpm=01;31:*.jar=01;31:*.war=01;31:*.ear=01;31:*.sar=01;31:*.rar=01;31:*.alz=01;31:*.ace=01;31:*.zoo=01;31:*.cpio=01;31:*.7z=01;31:*.rz=01;31:*.cab=01;31:*.jpg=01;35:*.jpeg=01;35:*.gif=01;35:*.bmp=01;35:*.pbm=01;35:*.pgm=01;35:*.ppm=01;35:*.tga=01;35:*.xbm=01;35:*.xpm=01;35:*.tif=01;35:*.tiff=01;35:*.png=01;35:*.svg=01;35:*.svgz=01;35:*.mng=01;35:*.pcx=01;35:*.mov=01;35:*.mpg=01;35:*.mpeg=01;35:*.m2v=01;35:*.mkv=01;35:*.webm=01;35:*.ogm=01;35:*.mp4=01;35:*.m4v=01;35:*.mp4v=01;35:*.vob=01;35:*.qt=01;35:*.nuv=01;35:*.wmv=01;35:*.asf=01;35:*.rm=01;35:*.rmvb=01;35:*.flc=01;35:*.avi=01;35:*.fli=01;35:*.flv=01;35:*.gl=01;35:*.dl=01;35:*.xcf=01;35:*.xwd=01;35:*.yuv=01;35:*.cgm=01;35:*.emf=01;35:*.axv=01;35:*.anx=01;35:*.ogv=01;35:*.ogx=01;35:*.aac=00;36:*.au=00;36:*.flac=00;36:*.mid=00;36:*.midi=00;36:*.mka=00;36:*.mp3=00;36:*.mpc=00;36:*.ogg=00;36:*.ra=00;36:*.wav=00;36:*.axa=00;36:*.oga=00;36:*.spx=00;36:*.xspf=00;36:';
export LS_COLORS
