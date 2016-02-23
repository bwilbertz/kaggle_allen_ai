# Description
The main function of **nvao** package is to preprocess the text using **Stanford CoreNLP** toolkit, including tokenization, sentence split, POS tagging, and lemmatization. We provide the option to filter out some tokens depending on their POS tags, and take the lemmas as the output.

# Files
**QAsPair**: data structure of the question and answers
**AnnotateText**: process the text
**ProcessQA**: process the QAsPair file

# Usage
	
        gradle processTraining
        gradle processTest


# License

See LICENSE.md in the main directory of this project.
