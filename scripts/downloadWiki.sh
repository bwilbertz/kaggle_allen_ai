#!/bin/sh

mkdir tmp 

cd tmp

for i in `cat ../scripts/wiki-urls.txt`
do
  echo "Downloading $i..."
  wget $i
done
