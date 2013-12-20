# Here is my bashrc.

export TERM=xterm-256color
export CLICOLOR=1
export LSCOLORS=GxFxCxDxBxegedabagacad

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
  export MAVEN_OPTS="-Xmx512m -Xms256m -XX:PermSize=128m -XX:MaxPermSize=256m"
  export MAVEN_OPTS_DEBUG="${MAVEN_OPTS} -Xdebug -Xnoagent -Djava.compiler=NONE -Xrunjdwp:transport=dt_socket,address=8781,server=y,suspend=n"
  export JAVA_OPTS="${MAVEN_OPTS} -Duser.timezone=UTC -Djava.awt.headless=true"
  export M2="$M2_HOME/bin"
fi

# Ant
set_if_exists "ANT_HOME" "${ANT_HOME}"
if [ "${ANT_HOME}x" != "x" ]; then
  export ANT_OPTS="-Xms512M -Xmx2048M -Xss1M -XX:MaxPermSize=128M"
fi

if [ ! -z "$IVY2_REPO_DIR" ]; then
  export ANT_OPTS="-Divy.default.ivy.user.dir=${IVY2_REPO_DIR} ${ANT_OPTS}"
fi

# Leiningen, included in dotfiles/bin
export LEIN_HOME=~/bin/lein-home

# Scala, not sure what to do with this yet
set_if_exists "SCALA_HOME" "${SCALA_HOME}"
# sbt is in dotfiles/bin and should be setup

# editors
export EDITOR=et # see ~/bin
export ALTERNATE_EDITOR=vim

# for emacs on MacOS, make sure to look at
# https://gist.github.com/mjwall/3fe935a8becb60dd3c4c

# git
if [ "${GIT_COMPLETION_DIR}x" == "x" ]; then
  echo "No Git completion"
elif [ ! -e "${GIT_COMPLETION_DIR}/git-prompt.sh" ]; then
  echo "Git completion expecting git-prompt.sh"
else
  source "${GIT_COMPLETION_DIR}/git-completion.bash"
  source "${GIT_COMPLETION_DIR}/git-prompt.sh"
  export GIT_PS1_SHOWDIRTYSTATE=true
  export PS1='\[\e[1;32m\]\u@\h \[\e[1;33m\]\w\[\e[0m\]$(__git_ps1 " (%s)")\n\$> '
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
# magit alias uses the ec script, see ~/bin
alias magit='ec -e "(magit-status \"$(pwd)\")"'

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

# setup ~/bin
prepend_path "${HOME}/bin"

#PATH=$PATH:$HOME/.rvm/bin # Add RVM to PATH for scripting
if [ "${RBENV}x" != "x" ]; then
  prepend_path "${RBENV}/bin"
  eval "$(rbenv init -)"
fi

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
