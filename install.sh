#!/bin/bash
set -e

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
    echo "Linking doom emacs to ~/.emacs.d"
    ln -s $SRC_DIR/dotfiles/doom-emacs/ $HOME/.emacs.d
    $HOME/.emacs.d/bin/doom install
else
    echo "Doom emacs already installed"
fi

if [ ! -d "$HOME/org" ]; then
    ln -s "$HOME/Dropbox/org" $HOME/org
else
    echo "Org directory exists"
fi

if [ ! -L "$HOME/.yabairc" ]; then
    echo "Linking .yabairc"
    ln -s $SRC_DIR/dotfiles/.yabairc $HOME/.yabairc
else
    echo "yabairc already installed"
fi

if [ ! -L "$HOME/.skhdrc" ]; then
    echo "Linking .skdhrc"
    ln -s $SRC_DIR/dotfiles/.skhdrc $HOME/.skhdrc
else
    echo "skhdrc already installed"
fi


echo "Installing fish"
rm -rf $HOME/.config/fish
ln -s $SRC_DIR/dotfiles/fish $HOME/.config/fish
echo "Installed fish. Run the following to complete setup.\n\t$ echo /usr/local/bin/fish | sudo tee -a /etc/shells\n\tchsh -s /usr/local/bin/fish"
