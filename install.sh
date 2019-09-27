#!/bin/bash

export SRC_DIR=$(cd "$(dirname "$0")/.."; pwd)
echo $SRC_DIR

if [ ! -L "$HOME/.vimrc" ]; then
    echo "Linking .vimrc"
    ln -s $SRC_DIR/dotfiles/.vimrc $HOME/.vimrc
else
    echo "vim configuration already installed"
fi

if [ ! -L "$HOME/.tmux.conf" ]; then
    echo "Linking .tmux.conf"
    ln -s $SRC_DIR/dotfiles/.tmux.conf $HOME/.tmux.conf
else
    echo "tmux configuration already installed"
fi

if [ ! -d "$HOME/.tmux/plugins/tpm" ]; then
    echo "Cloning tmux-plugins"
    git clone https://github.com/tmux-plugins/tpm $HOME/.tmux/plugins/tpm
else 
    echo "tmux plugins already installed"
fi

if [ ! -L "$HOME/.emacs.d" ]; then
    echo "Linking emacs directory"
    ln -s $SRC_DIR/dotfiles/spacemacs $HOME/.emacs.d
    ln -s $SRC_DIR/dotfiles/.spacemacs $HOME/.spacemacs
    
    echo "Checking out development branch"
    (cd $HOME/.emacs.d && git checkout develop)
else
    echo "Spacemacs already linked"
fi


