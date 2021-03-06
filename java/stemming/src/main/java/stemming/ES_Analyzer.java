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
package stemming;

import static org.elasticsearch.common.settings.ImmutableSettings.settingsBuilder;

import java.io.IOException;

import org.apache.lucene.analysis.TokenStream;
import org.apache.lucene.analysis.tokenattributes.CharTermAttribute;
import org.elasticsearch.Version;
import org.elasticsearch.cluster.metadata.IndexMetaData;
import org.elasticsearch.common.inject.Injector;
import org.elasticsearch.common.inject.ModulesBuilder;
import org.elasticsearch.common.settings.Settings;
import org.elasticsearch.common.settings.SettingsModule;
import org.elasticsearch.env.Environment;
import org.elasticsearch.env.EnvironmentModule;
import org.elasticsearch.index.Index;
import org.elasticsearch.index.IndexNameModule;
import org.elasticsearch.index.analysis.AnalysisModule;
import org.elasticsearch.index.analysis.AnalysisService;
import org.elasticsearch.index.analysis.NamedAnalyzer;
import org.elasticsearch.index.settings.IndexSettingsModule;
import org.elasticsearch.indices.analysis.IndicesAnalysisModule;
import org.elasticsearch.indices.analysis.IndicesAnalysisService;


/**
 * @author bwilbertz
 */
public class ES_Analyzer {

	final private NamedAnalyzer analyzer;
	final private boolean extended;
	
	public ES_Analyzer(String lang) {
		Index index = new Index("test");
        Settings settings = settingsBuilder().put(IndexMetaData.SETTING_VERSION_CREATED, Version.CURRENT).build();
        Injector parentInjector = new ModulesBuilder().add(new SettingsModule(settings), new EnvironmentModule(new Environment(settings)), new IndicesAnalysisModule()).createInjector();
        Injector injector = new ModulesBuilder().add(
                new IndexSettingsModule(index, settings),
                new IndexNameModule(index),
                new AnalysisModule(settings, parentInjector.getInstance(IndicesAnalysisService.class)))
                .createChildInjector(parentInjector);

        AnalysisService analysisService = injector.getInstance(AnalysisService.class);

        if (lang.startsWith("x") ) {
        	extended = true;
        	lang = lang.substring(1, lang.length());
        } else {
        	extended = false;
        }
        
        this.analyzer = analysisService.analyzer(lang);
	}
	
	public String stem(String sentence) throws IOException {
		try {
			if ( ! extended ) 
				return stem_(sentence);
			
			final String lstr = sentence.toLowerCase();
			
			if ( lstr.contains(" not ") || lstr.contains(" no ")) {
				final String result = stem_(lstr.replaceAll(" not ", " xxnotxx ").replaceAll(" no ", " xxnoxx "));
				
				return result.replaceAll("xxnotxx", "not").replaceAll("xxnoxx", "no");
			} else {
				return stem_(sentence);
			}
			
		} catch (Exception e) {
			System.out.println("Exception for string: " + sentence);
			e.printStackTrace();
		}
		
		return "";
	}
	
	
	private String stem_(String sentence) throws IOException {
		try {
			TokenStream tokenStream = analyzer.tokenStream("foo", sentence);

			CharTermAttribute charTermAttribute = tokenStream.addAttribute(CharTermAttribute.class);

			tokenStream.reset();
			final StringBuffer sb = new StringBuffer();
			while (tokenStream.incrementToken()) {            
			    String term = charTermAttribute.toString();
			    sb.append(term).append(" ");
			}
			
			tokenStream.close();
			
			return sb.toString().trim();
		} catch (Exception e) {
			System.out.println("Exception for string: " + sentence);
			e.printStackTrace();
		}
		
		return "";
	}
	
}
