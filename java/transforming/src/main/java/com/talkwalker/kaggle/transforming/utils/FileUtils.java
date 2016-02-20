package com.talkwalker.kaggle.transforming.utils;

import java.io.IOException;
import java.nio.ByteBuffer;
import java.nio.charset.Charset;
import java.nio.file.DirectoryStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardOpenOption;
import java.util.ArrayList;
import java.util.List;

/**
 * Helper class for file operations
 * 
 * @author Christopher Schank
 *
 */
public class FileUtils {

  /**
   * Reads a file into a string with the given charset Example String content =
   * readFile("test.txt", StandardCharsets.UTF_8); String content =
   * readFile("test.txt", Charset.defaultCharset());
   *
   * @param path
   * @param encoding
   * @return the content of the file as a string object
   * @throws IOException
   */
  public static String readFile(String path, Charset encoding) throws IOException {
    byte[] encoded = Files.readAllBytes(Paths.get(path));
    return encoding.decode(ByteBuffer.wrap(encoded)).toString();
  }

  public static String readFile(Path path, Charset encoding) throws IOException {
    byte[] encoded = Files.readAllBytes(path);
    return encoding.decode(ByteBuffer.wrap(encoded)).toString();
  }

  public static List<String> fileListAsString(String directory) {
    return fileListAsString(Paths.get(directory));
  }

  public static List<String> fileListAsString(Path directory) {
    List<String> fileNames = new ArrayList<>();
    try (DirectoryStream<Path> directoryStream = Files.newDirectoryStream(directory)) {
      for (Path path : directoryStream) {
        fileNames.add(path.toString());
      }
    } catch (IOException ex) {
    }
    return fileNames;
  }

  public static List<Path> fileList(String directory) {
    return fileList(Paths.get(directory));
  }

  public static List<Path> fileList(Path directory) {
    List<Path> fileNames = new ArrayList<>();
    try (DirectoryStream<Path> directoryStream = Files.newDirectoryStream(directory)) {
      for (Path path : directoryStream) {
        fileNames.add(path);
      }
    } catch (IOException ex) {
    }
    return fileNames;
  }

  public static void writeFile(String path, Charset encoding, String content, boolean append) throws IOException {
    writeFile(Paths.get(path), encoding, content, append);
  }

  public static void writeFile(Path path, Charset encoding, String content, boolean append) throws IOException {
    ByteBuffer encoded = encoding.encode(content);
    byte[] byteArray = new byte[encoded.limit()];
    encoded.get(byteArray);

    Files.write(path, byteArray, (append ? StandardOpenOption.APPEND : StandardOpenOption.TRUNCATE_EXISTING), StandardOpenOption.CREATE);

  }
}
