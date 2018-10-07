#!/bin/bash -
set -e
set -u
set -o pipefail

CWD=$(readlink -f $(dirname "${BASH_SOURCE}"))


# Shell
ln -sf $CWD/bash-init.sh $HOME/.bash_profile
ln -sf $CWD/bash-init.sh $HOME/.bashrc


# Emacs
if [[ -e $HOME/.emacs.d ]] && [[ ! -L $HOME/.emacs.d ]]; then
    # Delete old configure
    /bin/rm -rf $HOME/.emacs.d
fi
ln -sf $CWD/emacs.d $HOME/.emacs.d


# Scheme
ln -sf $CWD/chicken-scheme-init.sh $HOME/.csirc


# Common Lisp
ln -sf $CWD/sbcl-init.lisp $HOME/.sbclrc
