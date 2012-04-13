package com.wordnik.client.model;

public class Definition {
  private String text = null;
  private String source = null;
  private String note = null;
  private String partOfSpeech = null;
  private PartOfSpeech partOfSpeechObj = null;
  public String getText() {
    return text;
  }
  public void setText(String text) {
    this.text = text;
  }

  public String getSource() {
    return source;
  }
  public void setSource(String source) {
    this.source = source;
  }

  public String getNote() {
    return note;
  }
  public void setNote(String note) {
    this.note = note;
  }

  public String getPartOfSpeech() {
    return partOfSpeech;
  }
  public void setPartOfSpeech(String partOfSpeech) {
    this.partOfSpeech = partOfSpeech;
  }

  public PartOfSpeech getPartOfSpeechObj() {
    return partOfSpeechObj;
  }
  public void setPartOfSpeechObj(PartOfSpeech partOfSpeechObj) {
    this.partOfSpeechObj = partOfSpeechObj;
  }

  @Override
  public String toString()  {
    StringBuilder sb = new StringBuilder();
    sb.append("class Definition {\n");
    sb.append("  text: ").append(text).append("\n");
    sb.append("  source: ").append(source).append("\n");
    sb.append("  note: ").append(note).append("\n");
    sb.append("  partOfSpeech: ").append(partOfSpeech).append("\n");
    sb.append("  partOfSpeechObj: ").append(partOfSpeechObj).append("\n");
    sb.append("}\n");
    return sb.toString();
  }
}
