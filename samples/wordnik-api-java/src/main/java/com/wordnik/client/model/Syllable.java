package com.wordnik.client.model;

public class Syllable {
  private String text = null;
  private int seq = 0;
  private String type = null;
  public String getText() {
    return text;
  }
  public void setText(String text) {
    this.text = text;
  }

  public int getSeq() {
    return seq;
  }
  public void setSeq(int seq) {
    this.seq = seq;
  }

  public String getType() {
    return type;
  }
  public void setType(String type) {
    this.type = type;
  }

  @Override
  public String toString()  {
    StringBuilder sb = new StringBuilder();
    sb.append("class Syllable {\n");
    sb.append("  text: ").append(text).append("\n");
    sb.append("  seq: ").append(seq).append("\n");
    sb.append("  type: ").append(type).append("\n");
    sb.append("}\n");
    return sb.toString();
  }
}
