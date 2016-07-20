#!/usr/bin/env bash

### Copy Files ###
# change to directory where this script is
cd "$(dirname "$0")"
mkdir -p ~/bin
cp --remove-destination ./bin/* ~/bin/
cp --remove-destination -r ./config/* ~/.config/
cp --remove-destination ./pythonrc ~/.pythonrc
cp --remove-destination ./inputrc ~/.inputrc
cp --remove-destination ./vimrc ~/.vimrc

### install stuff ###
sudo add-apt-repository ppa:neovim-ppa/unstable
sudo apt-get update

sudo apt-get install neovim

pip install pillow
pip


### install i3 ###
sudo apt-get install libxcb1-dev libxcb-keysyms1-dev libpango1.0-dev libxcb-util0-dev libxcb-icccm4-dev libyajl-dev libstartup-notification0-dev libxcb-randr0-dev libev-dev libxcb-cursor-dev libxcb-xinerama0-dev libxcb-xkb-dev libxkbcommon-dev libxkbcommon-x11-dev

tmpdir=`mktmp -d` && cd $tmpdir
git clone https://www.github.com/Airblader/i3 i3-gaps
cd i3-gaps
git checkout gaps && git pull
make
sudo make install


### Install Fonts ##
mkdir -p ~/.fonts
cd ~/.fonts
wget "https://github.com/powerline/fonts/blob/master/Hack/Hack-Regular.ttf?raw=true"
wget "https://github.com/FortAwesome/Font-Awesome/blob/master/fonts/FontAwesome.otf?raw=true"


### install vim stuff ##
git clone https://github.com/VundleVim/Vundle.vim.git ~/.vim/bundle/Vundle.vim
vim +PluginInstall +qall

# Use nvim instead of vim
sudo update-alternatives --install /usr/bin/vi vi /usr/bin/nvim 60
sudo update-alternatives --config vi
sudo update-alternatives --install /usr/bin/vim vim /usr/bin/nvim 60
sudo update-alternatives --config vim
sudo update-alternatives --install /usr/bin/editor editor /usr/bin/nvim 60
sudo update-alternatives --config editor

