/*
 * The MIT License (MIT)
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */
package com.talkwalker.kaggle.transforming.utils;

import java.io.IOException;
import java.io.InputStream;

import org.apache.tika.exception.TikaException;
import org.apache.tika.metadata.Metadata;
import org.apache.tika.parser.AutoDetectParser;
import org.apache.tika.sax.BodyContentHandler;
import org.apache.tika.sax.ToXMLContentHandler;
import org.xml.sax.ContentHandler;
import org.xml.sax.SAXException;

/**
 * Helper class to using Apache Tika to read ebooks into XHTML or plain text
 * 
 * @author Christopher Schank
 *
 */
public class DocumentReader {

  public String parseDocumentXHTML(InputStream documentStream) throws TikaException, SAXException, IOException {
    ContentHandler handler = new ToXMLContentHandler();

    parse(handler, documentStream);
    return handler.toString();
  }

  public String parseDocumentPlainText(InputStream documentStream) throws TikaException, SAXException, IOException {
    ContentHandler handler = new BodyContentHandler(-1);

    parse(handler, documentStream);
    return handler.toString();
  }

  private void parse(ContentHandler handler, InputStream documentStream) throws TikaException, SAXException, IOException {
    AutoDetectParser parser = new AutoDetectParser();
    Metadata metadata = new Metadata();
    parser.parse(documentStream, handler, metadata);
  }

}
