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

if [ ! -L "$HOME/.doom.d" ]; then
    echo "Linking emacs directory"
    ln -s $SRC_DIR/dotfiles/.doom.d $HOME/.doom.d
else
    echo "Doom emacs config already linked"
fi

if [ ! -L "$HOME/.emacs.d" ]; then
    ln -s $SRC_DIR/doom-emacs/ $HOME/.emacs.d
else
    echo "Doom emacs already installed"
fi

if [ ! -d "$HOME/org" ]; then
    ln -s "$HOME/Dropbox/org" $HOME/org
else
    echo "Org directory exists"
fi

