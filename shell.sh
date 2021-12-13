if [ $INSIDE_EMACS ]; then
    export EDITOR='emacsclient'
    export PAGER=$HOME/.emacs.d/.local/straight/repos/emacs-pager/emacs-pager
    export GIT_PAGER=$HOME/.emacs.d/.local/straight/repos/emacs-pager/emacs-pager
    # export TERM='xterm-256color'
fi
