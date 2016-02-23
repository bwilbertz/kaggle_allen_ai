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
package com.talkwalker.kaggle.transforming;

import java.io.IOException;
import java.nio.charset.Charset;
import java.nio.file.FileVisitResult;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.SimpleFileVisitor;
import java.nio.file.attribute.BasicFileAttributes;
import java.util.LinkedList;
import java.util.List;

import org.apache.tika.exception.TikaException;
import org.json.simple.JSONAware;
import org.json.simple.JSONObject;
import org.jsoup.Jsoup;
import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;
import org.xml.sax.SAXException;

import com.google.common.base.CharMatcher;
import com.google.common.base.Function;
import com.google.common.base.Joiner;
import com.google.common.collect.Iterables;
import com.talkwalker.kaggle.transforming.utils.DocumentReader;
import com.talkwalker.kaggle.transforming.utils.FileUtils;

/**
 * Another parser for books, however this one is not specific to CK12 and can
 * process arbitrary documents. After each processed document a separator is
 * written in the output file to allow for easier manual post-processing.
 * (Removing appendixes, references, etc...)
 *
 * @author Christopher Schank
 *
 */
public class DocumentParser {

  public static final int PARAGRAPH_THRESHOLD = 200;

  public static final String URL_PATTERN = "\\b(((ht|f)tp(s?)\\:\\/\\/|~\\/|\\/)|www.)" +
      "(\\w+:\\w+@)?(([-\\w]+\\.)+(com|org|net|gov" +
      "|mil|biz|info|mobi|name|aero|jobs|museum" +
      "|travel|[a-z]{2}))(:[\\d]{1,5})?" +
      "(((\\/([-\\w~!$+|.,=]|%[a-f\\d]{2})+)+|\\/)+|\\?|#)?" +
      "((\\?([-\\w~!$+|.,*:]|%[a-f\\d{2}])+=?" +
      "([-\\w~!$+|.,*:=]|%[a-f\\d]{2})*)" +
      "(&(?:[-\\w~!$+|.,*:]|%[a-f\\d{2}])+=?" +
      "([-\\w~!$+|.,*:=]|%[a-f\\d]{2})*)*)*" +
      "(#([-\\w~!$+|.,*:=]|%[a-f\\d]{2})*)?\\b";

  public static void main(String[] args) throws IOException {

    if (args.length != 2) {
      System.out.println("Usage: DocumentParser <input directory> <output file>");
      return;
    }

    Path inputRoot = Paths.get(args[0]);
    Path outputFile = Paths.get(args[1]);

    Files.walkFileTree(inputRoot, new FileVisitor(outputFile));
    System.out.println("Finished parsing documents");
  }

  public static List<BookDocument> parseBook(Path file) throws TikaException, SAXException, IOException {

    String content = new DocumentReader().parseDocumentXHTML(Files.newInputStream(file));

    Files.createDirectories(Paths.get("tmp", "textDocs"));
    Path htmlDocument = Paths.get("tmp", "htmlTmp", file.getFileName().toString() + ".html");
    FileUtils.writeFile(htmlDocument, Charset.forName("UTF-8"), content, false);

    Document document = Jsoup.parse(content);
    Element body = document.body();

    List<BookDocument> docs = new LinkedList<>();

    String title = file.getFileName().toString();
    title = title.substring(0, title.lastIndexOf('.'));
    StringBuilder builder = new StringBuilder();
    for (Element paragraph : body.getElementsByTag("p")) {
      if (!paragraph.hasText()) {
        continue;
      }

      // Try to dected INDEX or REFERENCES sections and stop parsing
      if (paragraph.text().contains("INDEX") || paragraph.text().contains("REFERENCES")) {
        if (paragraph.text().contains("INDEX")) {
          builder.append(paragraph.text().substring(0, paragraph.text().indexOf("INDEX")));
        } else {
          builder.append(paragraph.text().substring(0, paragraph.text().indexOf("REFERENCES")));
        }

        builder.append(" ");
        docs.add(new BookDocument(title, builder.toString()));
        builder = new StringBuilder();
        break;
      }

      builder.append(paragraph.text());
      builder.append(" ");
      if (builder.length() > PARAGRAPH_THRESHOLD) {
        docs.add(new BookDocument(title, builder.toString()));
        builder = new StringBuilder();
      }
    }

    if (builder.length() > 0) {
      docs.add(new BookDocument(title, builder.toString()));
    }
    System.out.println("Found " + docs.size() + " documents for " + title);
    return docs;
  }

  public static void writeDocumentsToFile(List<BookDocument> documents, Path file) throws IOException {
    if (documents.isEmpty()) {
      return;
    }
    String output = Joiner.on('\n').join(Iterables.transform(documents, new Function<BookDocument, String>() {

      @Override
      public String apply(BookDocument input) {
        return input.toJSONString().trim();
      }

    }));

    // Add separator
    output = output + "\n\n========NEXT BOOK=========\n\n";
    FileUtils.writeFile(file, Charset.forName("UTF-8"), output, true);
  }

  private static class FileVisitor extends SimpleFileVisitor<Path> {

    private final Path outputPath;

    public FileVisitor(Path outputPath) {
      this.outputPath = outputPath;
    }

    @Override
    public FileVisitResult visitFile(Path file, BasicFileAttributes attrs) {
      if (file.toString().endsWith(".pdf")) {
        try {
          List<BookDocument> docs = parseBook(file);
          writeDocumentsToFile(docs, outputPath);
          System.out.println("Processed " + file.getFileName().toString());
        } catch (IOException | TikaException | SAXException e) {
          e.printStackTrace();
        }
      }
      return FileVisitResult.CONTINUE;
    }
  }

  public static class BookDocument implements JSONAware {
    private String title;
    private String text;

    public BookDocument(String title, String text) {
      super();
      this.title = CharMatcher.BREAKING_WHITESPACE.replaceFrom(title, ' ').trim();
      this.text = CharMatcher.WHITESPACE.collapseFrom(CharMatcher.BREAKING_WHITESPACE.replaceFrom(text, ' ')
          .replaceAll(URL_PATTERN, " ")
          .replaceAll("This content is available for free at ", ""), ' ').trim();
    }

    @SuppressWarnings("unchecked")
    @Override
    public String toJSONString() {
      JSONObject obj = new JSONObject();

      obj.put("text", text);
      obj.put("title", title);

      return obj.toJSONString();
    }
  }
}
