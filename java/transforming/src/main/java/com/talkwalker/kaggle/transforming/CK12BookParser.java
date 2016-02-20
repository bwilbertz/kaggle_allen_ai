package com.talkwalker.kaggle.transforming;

import java.io.IOException;
import java.nio.charset.Charset;
import java.nio.file.FileVisitResult;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.SimpleFileVisitor;
import java.nio.file.StandardCopyOption;
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
 * Parser for CK12 books, outputs in JSON
 *
 * @author Christopher Schank
 *
 */
public class CK12BookParser {

  public static final int PARAGRAPH_THRESHOLD = 300;

  public static final String CONTENT_START = "www.ck12.org Chapter";

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

  public static void main(String[] args) {

    if (args.length != 2) {
      System.out.println("Usage: CK12BookParser <input directory> <output file>");
      return;
    }

    Path inputRoot = Paths.get(args[0]);
    Path outputFile = Paths.get(args[1]);

    try {
      Files.walkFileTree(inputRoot, new FileVisitor(outputFile));
    } catch (IOException e) {
      e.printStackTrace();
    }
  }

  /**
   * Tries to parse a given file into several BookDocuments
   * 
   * @param file
   * @return
   * @throws TikaException
   * @throws SAXException
   * @throws IOException
   */
  public static List<BookDocument> parseBook(Path file) throws TikaException, SAXException, IOException {

    String content = new DocumentReader().parseDocumentXHTML(Files.newInputStream(file));

    Files.createDirectories(Paths.get("tmp", "textDocs"));
    Path htmlDocument = Paths.get("tmp", "textDocs", file.getFileName().toString() + ".html");
    FileUtils.writeFile(htmlDocument, Charset.forName("UTF-8"), content, false);

    Document document = Jsoup.parse(content);
    Element body = document.body();

    List<BookDocument> docs = new LinkedList<>();

    String title = null;
    StringBuilder builder = null;
    for (Element paragraph : body.getElementsByTag("p")) {
      if (!paragraph.hasText()) {
        continue;
      }
      if (title == null) {
        title = paragraph.text();
      } else {

        if (builder != null) {
          builder.append(paragraph.text());
          builder.append(" ");
          if (builder.length() > PARAGRAPH_THRESHOLD) {
            docs.add(new BookDocument(title, builder.toString()));
            builder = new StringBuilder();
          }
        } else {
          String removeFrom = CharMatcher.WHITESPACE.removeFrom(paragraph.text());
          if (removeFrom.contains("www.ck12.orgChapter")
              || removeFrom.contains("www.ck12.org1")
              || removeFrom.contains("www.ck12.orgConcept")) {
            builder = new StringBuilder();
          }
        }
      }
    }

    if (builder != null && builder.length() > 0) {
      docs.add(new BookDocument(title, builder.toString()));
    }
    if (builder == null) {
      Files.createDirectories(Paths.get("tmp", "errorDocs"));
      Files.copy(file, Paths.get("tmp", "errorDocs", file.getFileName().toString()), StandardCopyOption.REPLACE_EXISTING);
      Files.copy(htmlDocument, Paths.get("tmp", "errorDocs", htmlDocument.getFileName().toString()), StandardCopyOption.REPLACE_EXISTING);
    }

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
      this.text = CharMatcher.BREAKING_WHITESPACE.replaceFrom(text, ' ').replaceAll(URL_PATTERN, " ").trim();
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
