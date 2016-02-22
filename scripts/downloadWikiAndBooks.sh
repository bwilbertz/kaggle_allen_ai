#!/bin/sh

mkdir tmp 

cd tmp

for i in `cat ../scripts/links.txt`
do
  echo "Downloading $i..."
  wget $i
done
