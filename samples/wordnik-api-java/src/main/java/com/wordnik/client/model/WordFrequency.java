package com.wordnik.client.model;

public class WordFrequency {
  private long count = 0L;
  private String wordstring = null;
  public long getCount() {
    return count;
  }
  public void setCount(long count) {
    this.count = count;
  }

  public String getWordstring() {
    return wordstring;
  }
  public void setWordstring(String wordstring) {
    this.wordstring = wordstring;
  }

  @Override
  public String toString()  {
    StringBuilder sb = new StringBuilder();
    sb.append("class WordFrequency {\n");
    sb.append("  count: ").append(count).append("\n");
    sb.append("  wordstring: ").append(wordstring).append("\n");
    sb.append("}\n");
    return sb.toString();
  }
}
