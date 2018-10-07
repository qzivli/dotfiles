# If not running interactively, don't do anything
case $- in
    *i*) ;;
    *) return;;
esac



# For Guile Scheme
export LDFLAGS="-L/usr/local/opt/readline/lib"
export CPPFLAGS="-I/usr/local/opt/readline/include"


# For Chez Scheme

source ~/Documents/GitHub/chez-completion/chez-completion.bash



# Aliases
alias list-files="/usr/local/bin/ls --color=auto"

function delete-file () {
    local from=$1
    local to=~/.Trash/

    if [[ "$from" = "" ]]; then
        printf "ERROR: delete-file: \$1 is empty\n" >&2
        return 1
    fi

    if [[ ! -e "$from" ]]; then
        printf "ERROR: delete-file: $1 not exist\n" >&2
        return 2
    fi

    /usr/local/bin/mv $from $to
}


function rm () {
    printf "I'll drop using the command 'rm', sorry!\n" >&2
    printf "Instead, you can move file to ~/.Trash.\n" >&2
    printf "If you really want it, use the full path to 'rm', the command 'type -a rm' will help you to find the path.\n" >&2
    return 1
}




export PATH="/Applications/Racket v7.0/bin":$PATH
export CHEZSCHEMELIBDIRS=::~/scheme/libraries::~/lib/scheme::~/github/chez-bundle/src:

export HISTCONTROL=ignoredups:erasedups  # no duplicate entries
export HISTSIZE=100000                   # many commands in ongoing session memory
export HISTFILESIZE=100000               # many lines in .bash_history
shopt -s histappend                      # append to history, don't overwrite it

function reload() {
    source ~/.bash_profile
}

function site-mirror() {
    local url="$1"
    /usr/local/Cellar/wget/1.19.5/bin/wget --mirror --convert-links --adjust-extension --page-requisites --no-parent "$url"
}


function create-sh() {
    local filename="$1"

    if [[ $filename = "" ]]; then
        echo "usage: create-sh filename" >&2
        return 1
    fi

    cat > $filename <<EOF
#!/bin/bash
# $filename, create at $(date +%Y-%m-%d)

set -e

# cwd="\$(dirname "\$0")"

# trap '(read -p "[\$BASH_SOURCE:\$LINENO] \$BASH_COMMAND?")' DEBUG

EOF
}


function mit-scheme() {
    command "/Applications/MIT:GNU Scheme.app/Contents/Resources/mit-scheme" "$@"
}



function build-full-path () {
    return 0
}

function is-realative-path () {
    return 0
}


function append-trash-database () {
    return 0
}


function read-trash-database () {
    return 0
}


# function delete-file () {
#     guile ~/nanoshell/delete-file.go "$1"
# }



alias wget="/usr/local/Cellar/wget/1.19.5/bin/wget"
alias alisp="/Applications/AllegroCLexpress.app/Contents/Resources/alisp"
alias ccl="${HOME}/local/ccl/dx86cl64"
