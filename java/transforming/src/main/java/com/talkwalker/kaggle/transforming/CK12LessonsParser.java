package com.talkwalker.kaggle.transforming;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.nio.charset.Charset;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.LinkedList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.apache.tika.exception.TikaException;
import org.jsoup.Jsoup;
import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;
import org.xml.sax.SAXException;

import com.google.common.base.CharMatcher;
import com.google.common.base.Function;
import com.google.common.base.Joiner;
import com.google.common.base.Splitter;
import com.google.common.collect.Iterables;
import com.talkwalker.kaggle.transforming.CK12BookParser.BookDocument;
import com.talkwalker.kaggle.transforming.utils.DocumentReader;
import com.talkwalker.kaggle.transforming.utils.FileUtils;

/**
 * Parser for the CK12 lessons
 *
 * @author Christopher Schank
 *
 */
public class CK12LessonsParser {

  public static final int PARAGRAPH_THRESHOLD = 300;

  private final static Pattern DOCUMENT_PATTERN = Pattern.compile("<html.*?>.*?</html.*?>", Pattern.DOTALL);

  public static void main(String[] args) throws IOException {
    if (args.length != 2) {
      System.out.println("Usage: CK12LessonsParser <input directory> <output file>");
      return;
    }

    Path inputRoot = Paths.get(args[0]);
    Path outputFile = Paths.get(args[1]);

    List<String> documents = readInputFile(inputRoot);

    int index = 0;
    List<BookDocument> docs = new LinkedList<>();
    for (String d : documents) {
      try {
        docs.addAll(parseBookText(d, Integer.toString(index)));
      } catch (Exception e) {
        e.printStackTrace();
      }

      index++;
    }

    CK12BookParser.writeDocumentsToFile(docs, outputFile);
  }

  public static List<String> readInputFile(Path file) throws IOException {
    String input = FileUtils.readFile(file, Charset.defaultCharset());

    Matcher matcher = DOCUMENT_PATTERN.matcher(input);

    List<String> documentList = new LinkedList<>();
    while (matcher.find()) {
      documentList.add(matcher.group());
    }
    System.out.println("Found " + documentList.size() + " documents");
    return documentList;
  }

  public static List<BookDocument> parseBook(String input, String title) throws TikaException, SAXException, IOException {

    String content = new DocumentReader().parseDocumentXHTML(new ByteArrayInputStream(input.getBytes()));

    // Files.createDirectories(Paths.get("tmp", "textDocs"));
    // Path htmlDocument = Paths.get("tmp", "textDocs",
    // file.getFileName().toString() + ".html");
    // FileUtils.writeFile(htmlDocument, Charset.forName("UTF-8"), content,
    // false);

    Document document = Jsoup.parse(content);
    Element body = document.body();

    List<BookDocument> docs = new LinkedList<>();

    StringBuilder builder = new StringBuilder();
    for (Element paragraph : body.getElementsByTag("p")) {
      if (!paragraph.hasText()) {
        continue;
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

    System.out.println("Found " + docs.size() + " section in book " + title);
    return docs;
  }

  public static List<BookDocument> parseBookText(String input, String title) throws TikaException, SAXException, IOException {

    String content = new DocumentReader().parseDocumentPlainText(new ByteArrayInputStream(input.getBytes()));

    Files.createDirectories(Paths.get("tmp", "lessons-txt"));
    Path htmlDocument = Paths.get("tmp", "lessons-txt",
        title + ".txt");
    FileUtils.writeFile(htmlDocument, Charset.forName("UTF-8"), content,
        false);

    Document document = Jsoup.parse(content);
    Element body = document.body();

    List<BookDocument> docs = new LinkedList<>();

    StringBuilder builder = new StringBuilder();
    for (String line : Splitter.on(CharMatcher.BREAKING_WHITESPACE).split(content)) {
      builder.append(line);
      builder.append(" ");
      if (builder.length() > PARAGRAPH_THRESHOLD) {
        docs.add(new BookDocument(title, builder.toString()));
        builder = new StringBuilder();
      }
    }

    if (builder.length() > 0) {
      docs.add(new BookDocument(title, builder.toString()));
    }

    System.out.println("Found " + docs.size() + " section in book " + title);
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

}
