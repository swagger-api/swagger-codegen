package com.wordnik.client.model;

public class WordObject {
  private long id = 0L;
  private String word = null;
  private String vulgar = null;
  public long getId() {
    return id;
  }
  public void setId(long id) {
    this.id = id;
  }

  public String getWord() {
    return word;
  }
  public void setWord(String word) {
    this.word = word;
  }

  public String getVulgar() {
    return vulgar;
  }
  public void setVulgar(String vulgar) {
    this.vulgar = vulgar;
  }

  @Override
  public String toString()  {
    StringBuilder sb = new StringBuilder();
    sb.append("class WordObject {\n");
    sb.append("  id: ").append(id).append("\n");
    sb.append("  word: ").append(word).append("\n");
    sb.append("  vulgar: ").append(vulgar).append("\n");
    sb.append("}\n");
    return sb.toString();
  }
}
