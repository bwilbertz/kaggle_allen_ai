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
package NVAO;


import java.io.BufferedReader;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;
import java.io.Writer;
import java.util.HashSet;
import java.util.List;
import java.util.Properties;
import java.util.Set;
import java.util.logging.Logger;

import edu.stanford.nlp.ling.CoreAnnotations;
import edu.stanford.nlp.ling.CoreAnnotations.LemmaAnnotation;
import edu.stanford.nlp.ling.CoreAnnotations.PartOfSpeechAnnotation;
import edu.stanford.nlp.ling.CoreLabel;
import edu.stanford.nlp.pipeline.Annotation;
import edu.stanford.nlp.pipeline.StanfordCoreNLP;
import edu.stanford.nlp.util.CoreMap;

/**
 * @author Rui Wang
 */
public class AnnotateText
{
	private static final Logger log = Logger.getGlobal();

	public static final String USAGE = "jre1.8.0_66/bin/java -jar ProcessWiki.jar "
	      + "/data/wikipedia-cleaned/enwiki-cleaned.txt "
	      + "/data/wikipedia-processed/enwiki-cleaned-NVAO.txt "
	      + "NVAO";

	public static void main(String[] args) throws IOException
	{
		String inputFile = "/home/dev/development/eclipseworkspace_local/MarsProject/data/testinput";
//		String inputFile = "/home/dev/development/eclipseworkspace_local/MarsProject/data/simplewiki-cleaned.txt";
		String outputFile = "/home/dev/development/eclipseworkspace_local/MarsProject/data/testoutput";
//		String outputFile = "/home/dev/development/eclipseworkspace_local/MarsProject/data/simplewiki-cleaned-NVA.txt";

		if (args.length > 0)
		{
			inputFile = args[0];
		}

		if (args.length > 1)
		{
			outputFile = args[1];
		}

		Set<String> POSSet = new HashSet<String>();

		if (args.length > 2)
		{
			POSSet = getPOSSet(args[2]);
		}
		else
		{
			System.out.println(USAGE);
			System.exit(1);
		}

		Properties props = new Properties();
//		props.put("annotators", "tokenize, cleanxml, ssplit, pos, lemma, ner, parse, relation, natlog");
//		props.put("annotators", "tokenize, cleanxml, ssplit, pos, lemma");
		props.put("annotators", "tokenize, ssplit, pos, lemma");
//		props.put("ner.model", "edu/stanford/nlp/models/ner/english.all.3class.distsim.crf.ser.gz");
//		props.put("ner.applyNumericClassifiers", "false");
		StanfordCoreNLP pipeline = new StanfordCoreNLP(props);
		PrintWriter out = new PrintWriter(new FileWriter(outputFile));

		AnnotateText processor = new AnnotateText();

		BufferedReader reader = new BufferedReader(new FileReader(inputFile));
		String line = "";

		long t1 = System.currentTimeMillis();
		while ((line = reader.readLine()) != null)
		{
			if (line.trim().length() != 0)
			{
			if (line.startsWith("<doc id=") || line.startsWith("</doc>"))
			{
				out.print(line);
			}
			else
			{
				Annotation annotation = new Annotation(line.trim());
				pipeline.annotate(annotation);
				processor.outputAnnotation(annotation, out, POSSet, false);
			}
			}
			out.println();
		}

		out.flush();
		out.close();

		reader.close();

		long t2 = System.currentTimeMillis();
		log.info("Time took: " + (t2 - t1) + "ms");
	}

	public static Set<String> getPOSSet(String param)
	{
		Set<String> POSSet = new HashSet<String>();
		if (param.contains("N"))
		{
			POSSet.add("NN");
			POSSet.add("NNS");
			POSSet.add("NNP");
			POSSet.add("NNPS");
		}

		if (param.contains("V"))
		{
			POSSet.add("VB");
			POSSet.add("VBD");
			POSSet.add("VBG");
			POSSet.add("VBN");
			POSSet.add("VBP");
			POSSet.add("VBZ");
		}

		if (param.contains("A"))
		{
			POSSet.add("JJ");
			POSSet.add("JJR");
			POSSet.add("JJS");
			POSSet.add("RB");
			POSSet.add("RBR");
			POSSet.add("RBS");
		}

		if (param.contains("O"))
		{
			POSSet.add("CD");
			POSSet.add("FW");
		}
		return POSSet;
	}

	public boolean outputAnnotation(Annotation annotation, Writer out, Set<String> POSSet, boolean isAnswer) throws IOException
	{
		boolean isEmpty = true;
		List<CoreMap> sentences = annotation.get(CoreAnnotations.SentencesAnnotation.class);
		// display each sentence in this annotation
		if (sentences != null)
		{
			for (int i = 0, sz = sentences.size(); i < sz; i++)
			{
				CoreMap sentence = sentences.get(i);
				List<CoreLabel> tokens = sentence.get(CoreAnnotations.TokensAnnotation.class);
				for (CoreLabel token : tokens)
				{
	//				out.print(token.toShorterString(tokenAnnotations));
					String toWrite = toWrite(token, POSSet, isAnswer);
					if (toWrite.trim().length() != 0)
					{
						isEmpty = false;
						out.write(toWrite);
						out.write(" ");
					}
				}
			}
		}
		return isEmpty;
	}

	private String toWrite(CoreLabel token, Set<String> POSSet, boolean isAnswer) throws IOException
	{
		StringBuffer returnString = new StringBuffer();
		String pos = token.get(PartOfSpeechAnnotation.class);
		String lemma = token.get(LemmaAnnotation.class);
		if (POSSet.isEmpty() || POSSet.contains(pos))
		{
			Set<String> lemmaSet = new HashSet<String>();
			lemmaSet.add(lemma);
			for (String lm : lemmaSet)
			{
			returnString.append(lm);
			returnString.append(" ");
			}
		}
		return returnString.toString().trim();
	}
}
