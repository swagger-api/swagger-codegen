package com.wordnik.client.model;

public class TextPron {
  private String raw = null;
  private int seq = 0;
  private String rawType = null;
  public String getRaw() {
    return raw;
  }
  public void setRaw(String raw) {
    this.raw = raw;
  }

  public int getSeq() {
    return seq;
  }
  public void setSeq(int seq) {
    this.seq = seq;
  }

  public String getRawType() {
    return rawType;
  }
  public void setRawType(String rawType) {
    this.rawType = rawType;
  }

  @Override
  public String toString()  {
    StringBuilder sb = new StringBuilder();
    sb.append("class TextPron {\n");
    sb.append("  raw: ").append(raw).append("\n");
    sb.append("  seq: ").append(seq).append("\n");
    sb.append("  rawType: ").append(rawType).append("\n");
    sb.append("}\n");
    return sb.toString();
  }
}
