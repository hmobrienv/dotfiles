#!/bin/bash

export SRC_DIR=$(cd "$(dirname "$0")/.."; pwd)
echo $SRC_DIR

if [ ! -L ~/.vimrc ]; then
    echo "Linking .vimrc"
    ln -s $SRC_DIR/dotfiles/.vimrc ~/.vimrc
else
    echo "vim configuration already installed"
fi

if [ ! -L ~/.tmux.conf ]; then
    echo "Linking .tmux.conf"
    ln -s $SRC_DIR/dotfiles/.tmux.conf ~/.tmux.conf
else
    echo "tmux configuration already installed"
fi

if [ ! -d ~/.tmux/plugins/tpm ]; then
    echo "Cloning tmux-plugins"
    git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm
else 
    echo "tmux plugins already installed"
fi

if [ ! -L ~/.emacs.d ]; then
    echo "Linking emacs directory"
    ln -s $SRCDIR/dotfiles/spacemacs ~/.emacs.d
else
    echo "Spacemacs already linked"
fi
