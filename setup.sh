#!/bin/bash

script_dir() {
        if [ -z "${SCRIPT_DIR}" ]; then
                # even resolves symlinks, see
                # http://stackoverflow.com/questions/59895/can-a-bash-script-tell-what-directory-its-stored-in
                    local SOURCE="${BASH_SOURCE[0]}"
        while [ -h "$SOURCE" ] ; do SOURCE="$(readlink "$SOURCE")"; done
                SCRIPT_DIR="$( cd -P "$( dirname "$SOURCE" )" && pwd )"
    fi
            echo "${SCRIPT_DIR}"
}

CODE_HOME=$(script_dir)

setup_file_link() {
    local source="$1"
    local destination="$2"
    if [ ! -e "${destination}" ]; then
        ln -s "${source}" "${destination}" &&  echo "Linked file ${source} to ${destination}"
    else
        echo "ERROR: File ${source} already exists"
    fi
}

setup_dir_link() {
    local source="$1"
    local destination="$2"
    if [ ! -d "${destination}" ]; then
        ln -s "${source}" "${destination}" && echo "Linked directory ${source} to ${destination}"
    else
        echo "ERROR: Directory ${source} already exists"
    fi
}
# create backup dir if it doesn't exist
if [ ! -d "${HOME}/.backup" ]; then
  echo "Creating ~/.backup directory"
  mkdir "${HOME}/.backup"
fi

# setup links
setup_file_link "${CODE_HOME}/bashrc" "${HOME}/.bashrc"
setup_file_link "${CODE_HOME}/bash_profile" "${HOME}/.bash_profile"
setup_file_link "${CODE_HOME}/gitconfig" "${HOME}/.gitconfig"
setup_file_link "${CODE_HOME}/screenrc" "${HOME}/.screenrc"
setup_dir_link "${CODE_HOME}/dotvim" "${HOME}/.vim"
setup_dir_link "${CODE_HOME}/dotemacs.d" "${HOME}/.emacs.d"
setup_dir_link "${CODE_HOME}/bash_completion.d" "${HOME}/.bash_completion.d"
setup_dir_link "${CODE_HOME}/bin" "${HOME}/bin"
setup_dir_link "${CODE_HOME}/atom" "${HOME}/.atom"
setup_file_link "${HOME}/.vim/.vimrc" "${HOME}/.vimrc"
setup_file_link "${CODE_HOME}/tmux.conf" "${HOME}/.tmux.conf"

echo "Read the bashrc file you are setting up on Mac to get Emacs running"

# to remove everything, run the following.  Not going to automate this, because it is too destructive
# rm "${HOME}/.bashrc" "${HOME}/.bash_profile" "${HOME}/.gitconfig" "${HOME}/.screenrc" "${HOME}/.vim" "${HOME}/.emacs.d" "${HOME}/bin" "${HOME}/.bash_completion.d" "${HOME}/.vimrc" "${HOME}/.tmux.conf"
