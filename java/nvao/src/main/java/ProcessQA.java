package NVAO;


import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.Writer;
import java.util.Properties;
import java.util.Set;
import java.util.logging.Logger;

import edu.stanford.nlp.pipeline.Annotation;
import edu.stanford.nlp.pipeline.StanfordCoreNLP;

public class ProcessQA
{
	private static final Logger log = Logger.getGlobal();

	public static final String USAGE = "jre1.8.0_66/bin/java -jar ProcessQA.jar "
	      + "/data/wikipedia-cleaned/training_set.tsv "
	      + "/data/wikipedia-processed/training_set.tsv.NVAO.out "
	      + "NVAO";

	public static void main(String[] args) throws IOException
	{
		String inputFile = "/home/dev/development/eclipseworkspace_local/MarsProject/data/training_set.tsv";
//		String inputFile = "/home/dev/development/eclipseworkspace_local/MarsProject/data/validation_set.tsv";
		String POSParam = "NVAO";
		String outputFile = inputFile + "." + POSParam + ".out";
//		String outputDIR = "/home/dev/development/eclipseworkspace_local/MarsProject/data/training_set/";

		if (args.length > 0)
		{
			inputFile = args[0];
		}
		if (args.length > 1)
		{
			outputFile = args[1];
		}
		if (args.length > 2)
		{
			POSParam = args[2];
		}
		else
		{
			System.out.println(USAGE);
			System.exit(1);
		}
		AnnotateText pwiki = new AnnotateText();
		processQAFile(pwiki, inputFile, outputFile, AnnotateText.getPOSSet(POSParam));

	}
	
	public static void processQAFile(AnnotateText pwiki, String inputFile, String outputFile, Set<String> POSSet)
	{
		Properties props = new Properties();
		props.put("annotators", "tokenize, ssplit, pos, lemma");
		StanfordCoreNLP pipeline = new StanfordCoreNLP(props);
		
		try
		{
			BufferedReader reader = new BufferedReader(new FileReader(inputFile));
			BufferedWriter writer = new BufferedWriter(new FileWriter(outputFile));
			String line = "";
			while ((line = reader.readLine()) != null)
			{
				if (line.startsWith("id")) {
					writer.write(line);
					writer.newLine();
					continue;
				}
				String[] items = line.split("\t");
				if (items.length == 7)
				{
					writer.write(items[0]);
					writer.write("\t");
					processText(pwiki, pipeline, items[1], writer, POSSet);
					writer.write("\t");
					writer.write(items[2]);
					writer.write("\t");
					processText(pwiki, pipeline, items[3], writer, POSSet);
					writer.write("\t");
					processText(pwiki, pipeline, items[4], writer, POSSet);
					writer.write("\t");
					processText(pwiki, pipeline, items[5], writer, POSSet);
					writer.write("\t");
					processText(pwiki, pipeline, items[6], writer, POSSet);
				}
				else if (items.length == 6)
				{
					writer.write(items[0]);
					writer.write("\t");
					processText(pwiki, pipeline, items[1], writer, POSSet);
					writer.write("\t");
					processText(pwiki, pipeline, items[2], writer, POSSet);
					writer.write("\t");
					processText(pwiki, pipeline, items[3], writer, POSSet);
					writer.write("\t");
					processText(pwiki, pipeline, items[4], writer, POSSet);
					writer.write("\t");
					processText(pwiki, pipeline, items[5], writer, POSSet);
				}
			writer.newLine();
			}
			reader.close();
			
			writer.flush();
			writer.close();
		}
		catch (IOException e)
		{
			log.info(e.getMessage());
			System.exit(1);
		}
	}
	
	private static void processText(AnnotateText pwiki, StanfordCoreNLP pipeline, String text, Writer writer, Set<String> POSSet) throws IOException
	{
		Annotation annotationQ = new Annotation(preprocessText(text));
		pipeline.annotate(annotationQ);
		if (pwiki.outputAnnotation(annotationQ, writer, POSSet, false))
		{
			writer.write(text);
		}
	}

	private static String preprocessText(String text)
	{
		return text.replaceAll("[,.!?;:]", "$0 ").replaceAll("\\s+", " ");
	}

}