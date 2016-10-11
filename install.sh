#!/usr/bin/env bash

### Copy Files ###
# change to directory where this script is
# TODO: Consider symlinking
cd "$(dirname "$0")"
mkdir -p ~/bin
cp --remove-destination ./bin/* ~/bin/
cp --remove-destination -r ./config/* ~/.config/
cp --remove-destination ./pythonrc ~/.pythonrc
cp --remove-destination ./inputrc ~/.inputrc
cp --remove-destination ./vimrc ~/.vimrc
cp --remove-destination ./Xresources ~/.Xresources

# TODO: Missing .xresources


### Prepare apt ###
# fish
sudo apt-add-repository ppa:fish-shell/release-2
# spotify
sudo apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv-keys BBEBDCB318AD50EC6865090613B00F1FD2C19886
echo deb http://repository.spotify.com stable non-free | sudo tee /etc/apt/sources.list.d/spotify.list
# neovim
sudo add-apt-repository ppa:neovim-ppa/unstable
# make sure all changes are reflected in apt
sudo apt-get update


### i3 ###
# install dependencies
sudo apt-get install libxcb1-dev libxcb-keysyms1-dev libpango1.0-dev libxcb-util0-dev libxcb-icccm4-dev libyajl-dev libstartup-notification0-dev libxcb-randr0-dev libev-dev libxcb-cursor-dev libxcb-xinerama0-dev libxcb-xkb-dev libxkbcommon-dev libxkbcommon-x11-dev

# install i3-gaps
tmpdir=`mktemp -d` && cd $tmpdir
git clone https://www.github.com/Airblader/i3 i3-gaps
cd i3-gaps
git checkout gaps && git pull
make
sudo make install

# install i3blocks
cd ..
git clone git://github.com/vivien/i3blocks
cd i3blocks
make clean debug
sudo make install

sudo apt-get install i3lock


### VIM ###
sudo apt-get install neovim
git clone https://github.com/VundleVim/Vundle.vim.git ~/.vim/bundle/Vundle.vim
vim +PluginInstall +qall

# Use nvim instead of vim
sudo update-alternatives --install /usr/bin/vi vi /usr/bin/nvim 60
sudo update-alternatives --config vi
sudo update-alternatives --install /usr/bin/vim vim /usr/bin/nvim 60
sudo update-alternatives --config vim
sudo update-alternatives --install /usr/bin/editor editor /usr/bin/nvim 60
sudo update-alternatives --config editor


### Fish ###
sudo apt-get install fish
# TODO: make default shell


### Misc Programs ###
sudo apt-get install spotify-client

# Used for lock script
sudo apt-get install scrot imagemagick
pip install --upgrade pip
pip install pillow xcffib pyscreenshot

# Temp file to store installers
tmpdir=`mktemp -d` && cd $tmpdir

# rofi - the launcher
wget "https://launchpad.net/ubuntu/+source/rofi/0.15.7-1/+build/7645955/+files/rofi_0.15.7-1_amd64.deb"
sudo dpkg -i rofi_0.15.7-1_amd64.deb

# playerctl - for spotify information
wget "https://github.com/acrisci/playerctl/releases/download/v0.5.0/playerctl-0.5.0_amd64.deb"
sudo dpkg -i playerctl-0.5.0_amd64.deb


### Fonts ##
mkdir -p ~/.fonts
cd ~/.fonts
wget "https://github.com/powerline/fonts/blob/master/Hack/Hack-Regular.ttf?raw=true"
wget "https://github.com/FortAwesome/Font-Awesome/blob/master/fonts/FontAwesome.otf?raw=true"

# TODO: Make terminal use these fonts

