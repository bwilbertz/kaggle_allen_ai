# kaggle submission of the team 'poweredByTalkwalker' for the [Allen AI competition](https://www.kaggle.com/c/the-allen-ai-science-challenge)

This code allows to reproduce our solution, which scored 0.5900 on the public leaderboard and 0.58344 on the private one from Friday, 12th February 2016, 07:28:58 UTC.
We worked on an i7 quad-core machine with 32GB RAM and 400GB SSD disk running Ubuntu 14.04.3 LTS.

## model execution

This part has as dependencies only R(3.2.2) with packages plyr(1.8.3), dplyr(0.4.3), reshape2(1.4.1), caret(6.0-57) and xgboost(0.4-2).

In order to reproduce our submission, place the training\_set.tsv, validation\_set.tsv and test\_set.tsv files into the R/input folder.
Then execute

	./scripts/unsplitRData.sh

followed by

	cd R && mkdir output && R --no-save < createInputFile.R && R --no-save < runModel.R

This will produce the final submission in the folder R/output.

## full model prediction pipeline

In order to explore the model prediction in detail, we here also provide the code to build the IR retrieval indices from scratch. Be aware, that this part will download more than 25GB of data, requires a running ElasticSearch installation with around 100 GB of disk space and the runtime of all scripts together will be around 5 days.

As system requirements this part needs in addition

- most of the times Java 7 (Java 8 for NVAO transformation)
- ElasticSearch installation 1.7.3
- gradle 2.11
- python 2.7 + NLTK(3.1)

and R packages

- data.table(1.9.6)
- elastic(0.5.0)
- elasticdsl(0.0.3.9500)
- FeatureHashing(0.9.1.1)
- hash(2.2.6)
- httr(1.0.0)
- jsonlite(0.9.17)
- Matrix(1.2-2)
- rJava(0.9-7)
- RWeka(0.4-24)
- stringr(1.0.0)
- text2vec(0.2.1)
- tm(0.6-2)

(moreover Apache Tika, JSOUP, Stanford NLP library will be automatically downloaded by the gradle scripts)
 

### data preparation

- execute download scripts from the folder scripts
- download and install [WikiExtractor](https://github.com/attardi/wikiextractor/tree/9229e50bb3fd33b3477393945edca1bc1fbae3fb) and transform the dumps downloaded from Wikipedia, e.g.

	WikiExtractor.py -o tmp/simplewiki -b 100G -s -ns Article --no-templates tmp/simplewiki-20151020-pages-articles.xml.bz2

- execute transformation scripts as described in java/transformation
- use quizlet.py to retrieve quizlet data (see also Readme in python folder)
- transform the quizlet data by executing R --no-save < R/utils/importQuizlet.R
- execute NVAO processing as described in java/nvao on the kaggle input files in R/input

### ElasticSearch index creation

- execute the lines in es/Readme.md

### R pipeline

- build the jar in java/stemming according to the Readme.md and copy the result from build/libs into R/lib

- run the R script

	cd R && R --no-save < runFullModelPipeline.R


## training

in order to re-train the model, execute

	cd R && R --no-save < trainModel.R

