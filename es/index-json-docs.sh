#!/bin/bash

: ${1?"Usage: $0 indexname"}

cd "$(dirname "$0")"

./stream2es-0.0.2-SNAPSHOT stdin \
  --target http://localhost:9200/$1/page \
  --replace \
  --log debug \
  --settings '
{
  "query": {
    "default_field": "text"
  },
  "analysis": {
    "analyzer": {
      "default": {
        "type": "english"
      }
    }
  },
  "similarity": {
    "default": {
      "type": "BM25"
    }
  }
}' \
  --mappings '
{
  "page": {
    "properties": {
      "text": {
        "type": "string",
        "index_options": "offsets"
      },
      "title": {
        "type": "string",
        "index_options": "offsets"
      }
    }
  } 
}'
