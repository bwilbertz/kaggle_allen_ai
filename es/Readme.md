# create ElasticSearch indices

(based on [stream2es](https://github.com/elastic/stream2es))

Example Usage: 

	cat example.json | ./index-json-docs.sh my-testing-index


input.json format:
- One json document per line
- If id is not provided uses automatically generated ids

	{"id":"testid", "text":"def"}
	{"text":"cdf"}
	{"text":"abc", "numeric_field": 1}

See index-json-docs.sh for index settings.
Currently uses analyzer english, bm25 similarity, 'page' type and default field 'text'
NOTE: Index is replaced (deleted) on script start.

------

## Indices needed for full pipeline

- wiki

	python ../python/wiki2json.py --by-paragraph ../tmp/simplewiki/AA/wiki_00 | ./index-json-docs.sh simplewiki-english-bm25-wiki2json-byparagraph
	python ../python/wiki2json.py --by-paragraph ../tmp/enwiki/AA/wiki_00 | ./index-json-docs.sh enwiki-english-bm25-wiki2json-byparagraph
	python ../python/wiki2json.py --by-paragraph ../tmp/enwikibooks/AA/wiki_00 | ./index-json-docs.sh enwikibooks-english-bm25-wiki2json-byparagraph

- ck12

	cat ../tmp/ck12.txt | ./index-json-docs.sh ck12-english-bm25
	cat ../tmp/ck12_lessons.txt | ./index-json-docs.sh ck12_lessons-english-bm25
	
- books

	cat ../tmp/books.txt | ./index-json-docs.sh books-english-bm25

- quizlet

	cat ../tmp/quizlet.json | ./index-json-docs.sh quizlet-english-bm25
	cat ../tmp/quizlet_20160126.json | ./index-json-docs.sh quizlet_20160126-english-bm25

