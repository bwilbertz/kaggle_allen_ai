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

/**
 * @author Rui Wang
 */
public class QAsPair
{
	private final String id;
	private final String question;
	private final String answerA;
	private final String answerB;
	private final String answerC;
	private final String answerD;
	private final Choice correctAnswer;

	public String getId()
	{
		return id;
	}

	public String getQuestion()
	{
		return question;
	}

	public String getAnswerA()
	{
		return answerA;
	}

	public String getAnswerB()
	{
		return answerB;
	}

	public String getAnswerC()
	{
		return answerC;
	}

	public String getAnswerD()
	{
		return answerD;
	}

	public Choice getCorrectAnswer()
	{
		return correctAnswer;
	}

	private QAsPair(String id, String question, String answerA, String answerB, String answerC, String answerD, Choice correctAnswer)
	{
		super();
		this.id = id;
		this.question = question;
		this.answerA = answerA;
		this.answerB = answerB;
		this.answerC = answerC;
		this.answerD = answerD;
		this.correctAnswer = correctAnswer;
	}

	@Override
	public String toString()
	{
		return "Question: " + question + "; AnswerA: " + answerA + "; AnswerB: " + answerB + "; AnswerC: " + answerC + "; AnswerD: " + answerD;
	}

	public static QAsPair createQuestion(String id, String question, String answerA, String answerB, String answerC, String answerD)
	{
		return new QAsPair(id, question, answerA, answerB, answerC, answerD, Choice.Unknown);
	}

	public static QAsPair createCorrectAnswer(String id, String question, String answerA, String answerB, String answerC, String answerD,
	      Choice correctAnswer)
	{
		return new QAsPair(id, question, answerA, answerB, answerC, answerD, correctAnswer);
	}

	public enum Choice
	{
		A, B, C, D, Unknown
	}
}
