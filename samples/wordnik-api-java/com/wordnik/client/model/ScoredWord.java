package com.wordnik.client.model;

public class ScoredWord {
  private long id = 0L;
  private int position = 0;
  private String lemma = null;
  private int docTermCount = 0;
  private String wordType = null;
  private float score = 0.0f;
  private String word = null;
  private long sentenceId = 0L;
  private boolean stopword = false;
  private double baseWordScore = 0.0;
  private String partOfSpeech = null;
  public long getId() {
    return id;
  }
  public void setId(long id) {
    this.id = id;
  }

  public int getPosition() {
    return position;
  }
  public void setPosition(int position) {
    this.position = position;
  }

  public String getLemma() {
    return lemma;
  }
  public void setLemma(String lemma) {
    this.lemma = lemma;
  }

  public int getDocTermCount() {
    return docTermCount;
  }
  public void setDocTermCount(int docTermCount) {
    this.docTermCount = docTermCount;
  }

  public String getWordType() {
    return wordType;
  }
  public void setWordType(String wordType) {
    this.wordType = wordType;
  }

  public float getScore() {
    return score;
  }
  public void setScore(float score) {
    this.score = score;
  }

  public String getWord() {
    return word;
  }
  public void setWord(String word) {
    this.word = word;
  }

  public long getSentenceId() {
    return sentenceId;
  }
  public void setSentenceId(long sentenceId) {
    this.sentenceId = sentenceId;
  }

  public boolean getStopword() {
    return stopword;
  }
  public void setStopword(boolean stopword) {
    this.stopword = stopword;
  }

  public double getBaseWordScore() {
    return baseWordScore;
  }
  public void setBaseWordScore(double baseWordScore) {
    this.baseWordScore = baseWordScore;
  }

  public String getPartOfSpeech() {
    return partOfSpeech;
  }
  public void setPartOfSpeech(String partOfSpeech) {
    this.partOfSpeech = partOfSpeech;
  }

  @Override
  public String toString()  {
    StringBuilder sb = new StringBuilder();
    sb.append("class ScoredWord {\n");
    sb.append("  id: ").append(id).append("\n");
    sb.append("  position: ").append(position).append("\n");
    sb.append("  lemma: ").append(lemma).append("\n");
    sb.append("  docTermCount: ").append(docTermCount).append("\n");
    sb.append("  wordType: ").append(wordType).append("\n");
    sb.append("  score: ").append(score).append("\n");
    sb.append("  word: ").append(word).append("\n");
    sb.append("  sentenceId: ").append(sentenceId).append("\n");
    sb.append("  stopword: ").append(stopword).append("\n");
    sb.append("  baseWordScore: ").append(baseWordScore).append("\n");
    sb.append("  partOfSpeech: ").append(partOfSpeech).append("\n");
    sb.append("}\n");
    return sb.toString();
  }
}
