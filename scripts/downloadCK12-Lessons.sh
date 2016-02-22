#!/bin/sh

mkdir -p tmp/ck12_lessons 

cd tmp/ck12_lessons

for i in `cat ../scripts/ck12-lessons-urls.txt`
do
  echo "Downloading $i..."
  wget $i
done
