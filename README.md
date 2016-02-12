[![License GPL 3](https://img.shields.io/:license-GPLv3-blue.svg)](http://www.gnu.org/licenses/gpl-3.0.txt)
[![Build Status](https://travis-ci.org/sandor-juhasz/huninet-mode.svg?branch=master)](https://travis-ci.org/sandor-juhasz/huninet-mode)
[![Coverage Status](https://coveralls.io/repos/github/sandor-juhasz/huninet-mode/badge.svg?branch=master)](https://coveralls.io/github/sandor-juhasz/huninet-mode?branch=master)

# Huninet mode for Emacs

Copyright © 2016 Sándor Juhász

This minor mode is used to convert a text buffer containing Hungarian
text to the 7-bit "dead key" ("repülőékezetes") notation using the
Huninet recommendation. The recommendation can be found here:
http://deneb.iszt.hu/~pasztor/ekezet.html

## Vagrant environment

The attached Vagrantfile can be used to spawn a virtual machine with
all the necessary tools to compile, test and run the minor mode. The
box installs the following packages:
- evm
- emacs 24.5 from source code
- cask
- GNU automake, autoconf and other standard dev tools

Distributed under the GNU GPL version 3 or later.