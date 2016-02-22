#!/bin/sh

mkdir -p tmp/ck12 

cd tmp/ck12

for i in `cat ../scripts/ck12-books-urls.txt`
do
  echo "Downloading $i..."
  wget $i
done
