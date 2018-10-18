# For Chez Scheme
source ~/Documents/GitHub/chez-completion/chez-completion.bash
export CHEZSCHEMELIBDIRS=.:$HOME/Library/chezscheme/current

# For Racket
export PATH="/Applications/Racket v7.0/bin":$PATH

# For Chicken Scheme
alias csi='csi -R r7rs -q -w'

# For Guile Scheme
export LDFLAGS="-L/usr/local/opt/readline/lib"
export CPPFLAGS="-I/usr/local/opt/readline/include"


# For OCaml
eval $(opam env)



# If not running interactively, don't do anything
case $- in
    *i*) ;;
    *) return;;
esac

# Below only for interactive use.

case $(uname -s) in
    Darwin)
        alias ls='/usr/local/bin/ls --color=auto'
        alias wget="/usr/local/Cellar/wget/1.19.5/bin/wget"
        alias alisp="/Applications/AllegroCLexpress.app/Contents/Resources/alisp"
        alias ccl="${HOME}/local/ccl/dx86cl64"
        ;;
    *)
        alias ls='ls --color=auto'
        ;;
esac


GREEN="\[$(tput setaf 2)\]"
RESET="\[$(tput sgr0)\]"

export PS1="\n\$?\n\u@\h:\w \n${GREEN}:-)${RESET} "

function mit-scheme() {
    command "/Applications/MIT:GNU Scheme.app/Contents/Resources/mit-scheme" "$@"
}

function site-mirror() {
    local url="$1"
    wget --mirror --convert-links --adjust-extension --page-requisites --no-parent "$url"
}


export HISTCONTROL=ignoredups:erasedups  # no duplicate entries
export HISTSIZE=100000                   # many commands in ongoing session memory
export HISTFILESIZE=100000               # many lines in .bash_history
shopt -s histappend                      # append to history, don't overwrite it
