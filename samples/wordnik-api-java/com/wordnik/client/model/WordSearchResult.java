package com.wordnik.client.model;

public class WordSearchResult {
  private long count = 0L;
  private double lexicality = 0.0;
  private String word = null;
  public long getCount() {
    return count;
  }
  public void setCount(long count) {
    this.count = count;
  }

  public double getLexicality() {
    return lexicality;
  }
  public void setLexicality(double lexicality) {
    this.lexicality = lexicality;
  }

  public String getWord() {
    return word;
  }
  public void setWord(String word) {
    this.word = word;
  }

  @Override
  public String toString()  {
    StringBuilder sb = new StringBuilder();
    sb.append("class WordSearchResult {\n");
    sb.append("  count: ").append(count).append("\n");
    sb.append("  lexicality: ").append(lexicality).append("\n");
    sb.append("  word: ").append(word).append("\n");
    sb.append("}\n");
    return sb.toString();
  }
}
