# -*- mode: sh -*-

# If not running interactively, don't do anything
[[ $- == *i* ]] || return

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

# if ctags home, prepend path after emacs
# build universal ctags git@github.com:universal-ctags/ctags.git
# alias ctags to etags
if [ ! -z ${CTAGS_HOME} ]; then
  prepend_path ${CTAGS_HOME}/bin
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

# accumulo ulimit
#ulimit -S -n 65536

source ~/.dircolors
