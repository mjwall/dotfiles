# Here is my bashrc.  Lots of stuff here, can't guarantee it is the cleanest

export TERM=xterm-256color
export CLICOLOR=1
export LSCOLORS=GxFxCxDxBxegedabagacad

# Java and friends
if [ $(uname) == "Darwin" ]; then
  export JAVA_HOME=/Library/Java/Home # mac
else
  export JAVA_HOME=/usr/java/default # redhat
fi
export M2_HOME="${HOME}/opt/maven"
export MAVEN_OPTS="-Xmx512m -Xms256m -XX:PermSize=128m -XX:MaxPermSize=256m"
export MAVEN_OPTS_DEBUG="${MAVEN_OPTS} -Xdebug -Xnoagent -Djava.compiler=NONE -Xrunjdwp:transport=dt_socket,address=8781,server=y,suspend=n"
export JAVA_OPTS="${MAVEN_OPTS} -Duser.timezone=UTC -Djava.awt.headless=true -d32"
export M2="$M2_HOME/bin"
export ANT_HOME="${HOME}/opt/ant"
export ANT_OPTS="-Xms512M -Xmx2048M -Xss1M -XX:MaxPermSize=128M"
export GROOVY_HOME=/Library/Groovy/Home
export GRAILS_HOME=/Library/Grails/Home
export GANT_HOME=/Library/Gant/Home
export GRADLE_HOME=/Library/Gradle/Home
#export LEIN_HOME=~/opt/lein
#export SCALA_HOME=~/opt/scala
export PLAY_HOME=~/opt/play
#export SBT_HOME=~/opt/sbt
export TS_HOME="${HOME}/opt/typesafe-stack"
export PATH="${TS_HOME}/bin:${PATH}"
export SBT_HOME="${TS_HOME}/bin"

# Servers
export JETTY_HOME=${HOME}/opt/jetty
export JBOSS_HOME=${HOME}/opt/jboss
export APPENGINE_HOME=/Library/GoogleAppEngine/Home
export MONGO_HOME=${HOME}/opt/mongo

# Node
#export NODE_PATH=$HOME/local/node
#export NODE_BIN=/usr/local/share/npm/bin
#. ~/nvm/nvm.sh

# Python, installed with brew install python
if [ $(uname) == "Darwin" ]; then
  which brew && export PYTHONPATH="$(brew --prefix)/lib/python2.7/site-packages:$PYTHONPATH"
fi

# docbook
export XML_CATALOG_FILES="/usr/local/etc/xml/catalog"

# mactex
if [ $(uname) == "Darwin" ]; then
  # install mactex from http://www.tug.org/mactex/ and update accordingly
  export MACTEXT_HOME="/usr/local/texlive/2012/bin/x86_64-darwin"
  export PATH="${PATH}:${MACTEXT_HOME}"
fi

# ruby
# if [ -d "$HOME/.rvm" ]; then
#   PATH=$HOME/.rvm/bin:$PATH
#   if [ -s "$HOME/.rvmrc" ]; then
#     source "$HOME/.rvmrc"
#   fi # to have $rvm_path defined if set
#   if [ -s "${rvm_path-$HOME/.rvm}/scripts/rvm" ]; then
#     source "${rvm_path-$HOME/.rvm}/scripts/rvm"
#   fi
#   [[ -r /Users/mwall/.rvm/scripts/completion ]] && . /Users/mwall/.rvm/scripts/completion
# fi

# python
export PYTHONPATH="/usr/local/lib/python2.7/site-packages:$PYTHONPATH"

# editors
export EDITOR=et
export ALTERNATE_EDITOR=vim
#export GIT_EDITOR=emacsclient # for running git in ansi-term
# for emacs on MacOS, compile and install 23.4.1 into /Applications/Emacs.app
# chown -R root:staff /Applications/Emacs.app
#
# Add the following as /usr/bin/emacs (move old emacs if it is there)
# #!/bin/bash
# EMACS_PATH=/Applications/Emacs.app/Contents/MacOS/Emacs
#
# if [ $(id -u) = "0" ]; then
#   sudo $EMACS_PATH $*
# else
#   $EMACS_PATH $*
# fi
#
# symlink emacsclient
# sudo ln -s /Applications/Emacs.app/Contents/MacOS/bin/emacsclient /usr/bin/emacsclient

# git
# TODO: make this smarter, currently in ~/.bashrc-custom if different
if [ -d /usr/local/etc/bash_completion.d ]; then
  source /usr/local/etc/bash_completion.d/git-completion.bash
  source /usr/local/etc/bash_completion.d/git-prompt.sh
fi
export GIT_PS1_SHOWDIRTYSTATE=true
export PS1='\[\e[1;32m\]\u@\h \[\e[1;33m\]\w\[\e[0m\]$(__git_ps1 " (%s)")\n\$'

# from http://jonisalonen.com/2012/your-bash-prompt-needs-this/
# Seems to stop text response such as curl from showing up
#export PS1="\[\033[G\]$PS1"

# other completion scripts
source ~/.bash_completion.d/mvn-completion.bash
source ~/.bash_completion.d/svn-completion.bash

export PATH="${HOME}/bin:${HOME}/bin2:${JAVA_HOME}/bin:/usr/local/bin:$ANT_HOME/bin:$GRAILS_HOME/bin:$M2:$NODE_PATH/bin:$MONGO_HOME/bin:$SCALA_HOME/bin:$SBT_HOME/bin:$PLAY_HOME:$PATH:/usr/local/sbin"

alias gvim=mvim
alias gview="mvim -R"
alias sshaddme='ssh-add ~/.ssh/id_*sa.1'
alias ..="cd .."
alias work="screen -c ~/.work-screen"
alias tar="COPYFILE_DISABLE=true tar" # we don't need ._ files everywhere on a mac, see http://www.commandlinefu.com/commands/view/5965/create-.tar-file-on-mac-os-x-leopard-snow-leopard-without-._-files
alias edb="export EMISSARY_DBG_OPTIONS=\"-agentlib:jdwp=transport=dt_socket,server=y,suspend=y,address=5005\""
alias noedb="unset EMISSARY_DBG_OPTIONS"
alias magit='emacsclient -a emacs -e "(magit-status \"$(pwd)\")"'

#history stuff
shopt -s histappend
PROMPT_COMMAND='history -a'
shopt -s cdspell
export HISTCONTROL="ignoredups"
export HISTIGNORE="&:ls:[bf]g:exit"
shopt -s cmdhist

# standalone hadoop-1.0.4
#export HADOOP_PREFIX=/opt/hadoop-1.0.4
#export PATH="${HADOOP_PREFIX}/bin:${PATH}"

# standalone for hadoop-0.20.205.0
export HADOOP_PREFIX=/opt/hadoop-0.20.205.0
export PATH="${HADOOP_PREFIX}/bin:${PATH}"


# gpg agent
#export GPG_TTY='tty'
#if [ -f "${HOME}/.gpg-agent-info" ]; then
#  . "${HOME}/.gpg-agent-info"
#  export GPG_AGENT_INFO
#  # export SSH_AUTH_SOCK
#else
#  echo "Starting gpg-agent"
#fi

# accumulo stuff
#ulimit -n 2048
#source /opt/cloud/bin/cloud-env

# start up screen if it is not running
#if [ "$PS1" != "" -a "${STARTED_SCREEN:-x}" = x -a "${SSH_TTY:-x}" ]
#then
#  STARTED_SCREEN=1 ; export STARTED_SCREEN
#  sleep 1
#  screen -RR && exit 0
  # normally, execution of this rc script ends here...
#  echo "Screen failed! continuing with normal bash startup"
#fi

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

#PATH=$PATH:$HOME/.rvm/bin # Add RVM to PATH for scripting
export PATH="${HOME}/.rbenv/bin:$PATH"
eval "$(rbenv init -)"

# this bashrc-custom file is not committed, allowing for differences
# between installs.  Just create the file and edit with your needs
source ~/.bashrc-custom
