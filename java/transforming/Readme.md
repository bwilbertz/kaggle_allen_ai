# Transforming

## Description

This project is used to read different kinds of ebooks and HTML documents into the JSON format used by our system.
It contains the following main classes:

* CK12BookParser
* CK12LessonsParser
* DocumentParser

Both the **CK12BookParser** and **CK12LessonsParser** are more specialized version of the **DocumentParser** in the sense that they try to detect
common elements in the CK12 data and remove garbage data (like footnotes, introductions, etc...). **Apache Tika** is used to read the input documents into
either XHTML or plain text, depending on the requirements. For XTML **JSOUP** is then used to read the resulting document into a DOM tree after which the content is extracted by iterating over the *&lt;p&gt;* elements. In order to avoid creating a lot of very small output documents, the contents of the *&lt;p&gt;* elements are combined if they are below a certain threshold length. Additionally regexes are used to identify and remove URLs and other unwanted content.

## Output JSON Format

    {
      "title": "This is a title",
      "text": "This is the text"
    }

## Building & Running

	gradle runCK12BookParser -Dexec.args="<input-directory> <output-file>"
	gradle runCK12LessonsParser -Dexec.args="<input-directory> <output-file>"
	gradle runCK12DocumentParser -Dexec.args="<input-directory> <output-file>"
