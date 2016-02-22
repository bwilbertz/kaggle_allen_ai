package NVAO;

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
