#!/bin/bash

plugins=$(cat $1 | grep ^Plugin | cut -d ' ' -f2 | sed "s/^'\(.*\)'$/\1/")

mkdir -p ~/.vim/bundle

for plugin in ${plugins}; do
    echo "Installing $plugin..."

    clone_url="https://github.com/${plugin}"
    cd ~/.vim/bundle && git clone --quiet $clone_url
done
