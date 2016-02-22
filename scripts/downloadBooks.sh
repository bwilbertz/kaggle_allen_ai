#!/bin/sh

mkdir -p tmp/books 

cd tmp/books

for i in `cat ../scripts/openstax-wikibooks-urls.txt`
do
  echo "Downloading $i..."
  wget $i
done
