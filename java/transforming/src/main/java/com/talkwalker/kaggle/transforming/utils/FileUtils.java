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
