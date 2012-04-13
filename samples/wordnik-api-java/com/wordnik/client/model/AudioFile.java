package com.wordnik.client.model;

import java.util.Date;
public class AudioFile {
  private String attributionUrl = null;
  private int commentCount = 0;
  private int voteCount = 0;
  private String fileUrl = null;
  private String audioType = null;
  private long id = 0L;
  private double duration = 0.0;
  private String attributionText = null;
  private String createdBy = null;
  private String description = null;
  private Date createdAt = null;
  private float voteWeightedAverage = 0.0f;
  private float voteAverage = 0.0f;
  private String word = null;
  public String getAttributionUrl() {
    return attributionUrl;
  }
  public void setAttributionUrl(String attributionUrl) {
    this.attributionUrl = attributionUrl;
  }

  public int getCommentCount() {
    return commentCount;
  }
  public void setCommentCount(int commentCount) {
    this.commentCount = commentCount;
  }

  public int getVoteCount() {
    return voteCount;
  }
  public void setVoteCount(int voteCount) {
    this.voteCount = voteCount;
  }

  public String getFileUrl() {
    return fileUrl;
  }
  public void setFileUrl(String fileUrl) {
    this.fileUrl = fileUrl;
  }

  public String getAudioType() {
    return audioType;
  }
  public void setAudioType(String audioType) {
    this.audioType = audioType;
  }

  public long getId() {
    return id;
  }
  public void setId(long id) {
    this.id = id;
  }

  public double getDuration() {
    return duration;
  }
  public void setDuration(double duration) {
    this.duration = duration;
  }

  public String getAttributionText() {
    return attributionText;
  }
  public void setAttributionText(String attributionText) {
    this.attributionText = attributionText;
  }

  public String getCreatedBy() {
    return createdBy;
  }
  public void setCreatedBy(String createdBy) {
    this.createdBy = createdBy;
  }

  public String getDescription() {
    return description;
  }
  public void setDescription(String description) {
    this.description = description;
  }

  public Date getCreatedAt() {
    return createdAt;
  }
  public void setCreatedAt(Date createdAt) {
    this.createdAt = createdAt;
  }

  public float getVoteWeightedAverage() {
    return voteWeightedAverage;
  }
  public void setVoteWeightedAverage(float voteWeightedAverage) {
    this.voteWeightedAverage = voteWeightedAverage;
  }

  public float getVoteAverage() {
    return voteAverage;
  }
  public void setVoteAverage(float voteAverage) {
    this.voteAverage = voteAverage;
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
    sb.append("class AudioFile {\n");
    sb.append("  attributionUrl: ").append(attributionUrl).append("\n");
    sb.append("  commentCount: ").append(commentCount).append("\n");
    sb.append("  voteCount: ").append(voteCount).append("\n");
    sb.append("  fileUrl: ").append(fileUrl).append("\n");
    sb.append("  audioType: ").append(audioType).append("\n");
    sb.append("  id: ").append(id).append("\n");
    sb.append("  duration: ").append(duration).append("\n");
    sb.append("  attributionText: ").append(attributionText).append("\n");
    sb.append("  createdBy: ").append(createdBy).append("\n");
    sb.append("  description: ").append(description).append("\n");
    sb.append("  createdAt: ").append(createdAt).append("\n");
    sb.append("  voteWeightedAverage: ").append(voteWeightedAverage).append("\n");
    sb.append("  voteAverage: ").append(voteAverage).append("\n");
    sb.append("  word: ").append(word).append("\n");
    sb.append("}\n");
    return sb.toString();
  }
}
