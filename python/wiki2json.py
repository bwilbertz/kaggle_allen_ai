'''
Created on 17.11.2015

@author: benedikt
'''

import sys
import json
import os.path
import argparse
from nltk.tokenize import sent_tokenize

class article_streamer(object):
    def __init__(self, filename):
        self._filename = filename
        
    def __iter__(self):
        return self.delegate(self)
     
    class delegate(object):
        def __init__(self, outer):
            self._fid = open(outer._filename)
    
        def next(self):
            line = self._fid.readline()
            text = ''
            while not line.startswith('</doc'):
                if not line:
                    raise StopIteration
            
                if line.startswith('<doc id="'):
                    title = self._fid.readline().strip()                                        
                elif line != '\n':
                    text += ' ' + line.strip()
                
                line = self._fid.readline()

            return {'title' : title, 'text': text.strip() }

        def __del__(self):
            self._fid.close()

class paragraph_streamer(object):
    def __init__(self, filename):
        self._filename = filename
        
    def __iter__(self):
        return self.delegate(self)
     
    class delegate(object):
        def __init__(self, outer):
            self._fid = open(outer._filename)
    
        def next(self):
            line = self._fid.readline()
            while line == '\n' or line.startswith('</doc'):
                line = self._fid.readline()

            if not line:
                raise StopIteration

            if line.startswith('<doc id="'):
                self._title = self._fid.readline().strip()
                self._fid.readline() # skip another line
                line = self._fid.readline()

            return {'title' : self._title, 'text': line.strip() }

        def __del__(self):
            self._fid.close()
            
class sentence_streamer(object):
    def __init__(self, filename, n_sentences, overlap):
        #reload(sys)  
        #sys.setdefaultencoding('utf8')
        self._filename = filename
        self._n_sentences = n_sentences
        self._overlap = overlap
        
    def __iter__(self):
        return self.delegate(self)
     
    class delegate(object):
        def __init__(self, outer):
            self._n_sentences = outer._n_sentences
            self._overlap = outer._overlap
            self._fid = open(outer._filename)
            self._iterator = self._iterator()
    
        def next(self):
            return self._iterator.next()
            
        def _iterator(self):
            line = self._fid.readline()
            while line:
                while line == '\n' or line.startswith('</doc'):
                    line = self._fid.readline()
    
                if not line:
                    raise StopIteration
    
                if line.startswith('<doc id="'):
                    self._title = self._fid.readline().strip()
                    self._fid.readline() # skip another line
                    line = self._fid.readline()
                    
                sentences = sent_tokenize(unicode(line.strip(), 'utf-8'))
    
                if self._overlap:
                    for i in xrange(len(sentences)):
                        j = i+self._n_sentences 
                        if j <= len(sentences):
                            yield {'title' : self._title, 'text': " ".join(sentences[i:j]) }
                else:
                    for i in xrange(0, len(sentences), self._n_sentences):
                        j = min(i+self._n_sentences, len(sentences)) 
                        yield {'title' : self._title, 'text': " ".join(sentences[i:j]) }
                    
                line = self._fid.readline()

        def __del__(self):
            self._fid.close()
            
def parse_args():
    """Parse input arguments."""
    parser = argparse.ArgumentParser(description='Parse wikipedia text file into json for indexing')
    parser.add_argument('--by-paragraph', dest='by_paragraph',
                        help='Create one document per paragraph (in contrast to one document per article)',
                        action='store_true')
    parser.add_argument('--by-sentence', dest='n_sentences', type=int,
                        help='Create one document per N_SENTENCES sentences')
    parser.add_argument('--by-overlap-sentence', dest='n_over_sentences', type=int,
                        help='Create documents of overlaping sentences of length N_SENTENCES')
    parser.add_argument('filename', help='The wikipedia corpus to parse')

    args = parser.parse_args()

    return args
        


if __name__ == '__main__':
    args = parse_args()
    
    if args.by_paragraph:
        docs = paragraph_streamer(args.filename)
    elif args.n_sentences:
        docs = sentence_streamer(args.filename, args.n_sentences, False)
    elif args.n_over_sentences:
        docs = sentence_streamer(args.filename, args.n_over_sentences, True)
    else:
        docs = article_streamer(args.filename)
        
    for d in docs:
        print json.dumps(d)
    
