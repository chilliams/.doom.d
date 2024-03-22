# Don't put duplicate lines or lines starting with space in the history
# See bash(1) for more options
HISTCONTROL=ignoreboth:erasedups

# Append to the history file, don't overwrite it
shopt -s histappend
# Append and reload the history after each command
PROMPT_COMMAND="history -a; history -n"

# Ignore certain commands from the history
HISTIGNORE="ls:ll:cd:pwd:bg:fg:history"

# For setting history length see HISTSIZE and HISTFILESIZE in bash(1)
HISTSIZE=100000
HISTFILESIZE=10000000

# Always display the error code unless it’s 0
prompt_show_ec () {
    # Catch exit code
    ec=$?
    # Display exit code in red text unless zero
    if [ $ec -ne 0 ];then
        echo -e "\033[31;1m[$ec]\033[0m"
    fi
}

PROMPT_COMMAND="prompt_show_ec; $PROMPT_COMMAND"

# Show git branch in prompt
parse_git_branch() {
    git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/(\1)/'
}

PS1="\w\$(parse_git_branch)@\@\n$ "

GOPACKAGESDRIVER=$HOME/.doom.d/gopackagesdriver.sh

if [ $INSIDE_EMACS ]; then
    export EDITOR="emacsclient"
    export PAGER=$HOME/.config/emacs/.local/straight/repos/emacs-pager/emacs-pager
    export GIT_PAGER=$HOME/.config/emacs/.local/straight/repos/emacs-pager/emacs-pager
    export TERM="xterm-256color"
fi
