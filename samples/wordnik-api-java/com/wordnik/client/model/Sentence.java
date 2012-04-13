package com.wordnik.client.model;

import java.util.*;
public class Sentence {
  private long id = 0L;
  private boolean hasScoredWords = false;
  private List<ScoredWord> scoredWords = new ArrayList<ScoredWord>();
  private String display = null;
  private int rating = 0;
  private long documentMetadataId = 0L;
  public long getId() {
    return id;
  }
  public void setId(long id) {
    this.id = id;
  }

  public boolean getHasScoredWords() {
    return hasScoredWords;
  }
  public void setHasScoredWords(boolean hasScoredWords) {
    this.hasScoredWords = hasScoredWords;
  }

  public List<ScoredWord> getScoredWords() {
    return scoredWords;
  }
  public void setScoredWords(List<ScoredWord> scoredWords) {
    this.scoredWords = scoredWords;
  }

  public String getDisplay() {
    return display;
  }
  public void setDisplay(String display) {
    this.display = display;
  }

  public int getRating() {
    return rating;
  }
  public void setRating(int rating) {
    this.rating = rating;
  }

  public long getDocumentMetadataId() {
    return documentMetadataId;
  }
  public void setDocumentMetadataId(long documentMetadataId) {
    this.documentMetadataId = documentMetadataId;
  }

  @Override
  public String toString()  {
    StringBuilder sb = new StringBuilder();
    sb.append("class Sentence {\n");
    sb.append("  id: ").append(id).append("\n");
    sb.append("  hasScoredWords: ").append(hasScoredWords).append("\n");
    sb.append("  scoredWords: ").append(scoredWords).append("\n");
    sb.append("  display: ").append(display).append("\n");
    sb.append("  rating: ").append(rating).append("\n");
    sb.append("  documentMetadataId: ").append(documentMetadataId).append("\n");
    sb.append("}\n");
    return sb.toString();
  }
}
