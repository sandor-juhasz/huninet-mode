#!/usr/bin/env bash

apt-get update
apt-get -y install python-software-properties curl git

# The following lines are needed to build the Emacs on a
# 32 bit platform.

apt-get -y install build-essential libncurses-dev autoconf automake autogen texinfo
apt-add-repository -y ppa:brightbox/ruby-ng
apt-get update
apt-get -y install ruby2.2

mkdir /opt/evm
sudo chown vagrant: /opt/evm
echo "Please log in with ssh and run the vagrant/install-emacs.sh script!"
