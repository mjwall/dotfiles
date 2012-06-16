#!/bin/bash

CODE_HOME=${HOME}/src/Personal/dotfiles
# setup links
ln -s "${CODE_HOME}/bashrc" ~/.bashrc
ln -s "${CODE_HOME}/bash_profile" ~/.bash_profile
ln -s "${CODE_HOME}/gitconfig" ~/.gitconfig
ln -s "${CODE_HOME}/screenrc" ~/.screenrc
ln -s "${CODE_HOME}/dotvim" ~/.vim
ln -s ~/.vimrc ~/.vim/.vimrc
ln -s "${CODE_HOME}/dotemacs.d" ~/.emacs.d
ln -s "${CODE_HOME}/bin" ~/bin
ln -s "${CODE_HOME}/bash_completion.d" ~/.bash_completion.d
