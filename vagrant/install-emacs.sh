#!/bin/bash

curl -fsSkL https://raw.github.com/rejeep/evm/master/go | bash
export PATH="/home/vagrant/.evm/bin:$PATH"

echo $PATH

evm config path /opt/evm
evm install emacs-24.5
evm use emacs-24.5

emacs --version

curl -fsSL https://raw.githubusercontent.com/cask/cask/master/go | python
export PATH="/home/vagrant/.cask/bin:$PATH"

echo 'export PATH="/home/vagrant/.evm/bin:$PATH"' >>~/.profile
echo 'export PATH="/home/vagrant/.cask/bin:$PATH"' >>~/.profile

rm -r /opt/evm/tmp
