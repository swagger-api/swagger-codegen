package com.wordnik.client.model;

import java.util.Date;
public class WordListWord {
  private long id = 0L;
  private String username = null;
  private Date createdAt = null;
  private long userId = 0L;
  private long numberCommentsOnWord = 0L;
  private String word = null;
  private long numberLists = 0L;
  public long getId() {
    return id;
  }
  public void setId(long id) {
    this.id = id;
  }

  public String getUsername() {
    return username;
  }
  public void setUsername(String username) {
    this.username = username;
  }

  public Date getCreatedAt() {
    return createdAt;
  }
  public void setCreatedAt(Date createdAt) {
    this.createdAt = createdAt;
  }

  public long getUserId() {
    return userId;
  }
  public void setUserId(long userId) {
    this.userId = userId;
  }

  public long getNumberCommentsOnWord() {
    return numberCommentsOnWord;
  }
  public void setNumberCommentsOnWord(long numberCommentsOnWord) {
    this.numberCommentsOnWord = numberCommentsOnWord;
  }

  public String getWord() {
    return word;
  }
  public void setWord(String word) {
    this.word = word;
  }

  public long getNumberLists() {
    return numberLists;
  }
  public void setNumberLists(long numberLists) {
    this.numberLists = numberLists;
  }

  @Override
  public String toString()  {
    StringBuilder sb = new StringBuilder();
    sb.append("class WordListWord {\n");
    sb.append("  id: ").append(id).append("\n");
    sb.append("  username: ").append(username).append("\n");
    sb.append("  createdAt: ").append(createdAt).append("\n");
    sb.append("  userId: ").append(userId).append("\n");
    sb.append("  numberCommentsOnWord: ").append(numberCommentsOnWord).append("\n");
    sb.append("  word: ").append(word).append("\n");
    sb.append("  numberLists: ").append(numberLists).append("\n");
    sb.append("}\n");
    return sb.toString();
  }
}
