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
