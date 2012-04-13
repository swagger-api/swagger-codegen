package com.wordnik.client.model

import java.util.Date
import scala.reflect.BeanProperty

class AudioFile {
  @BeanProperty var attributionUrl: String = _
  @BeanProperty var commentCount: Int = 0
  @BeanProperty var voteCount: Int = 0
  @BeanProperty var fileUrl: String = _
  @BeanProperty var audioType: String = _
  @BeanProperty var id: Long = 0L
  @BeanProperty var duration: Double = 0.0
  @BeanProperty var attributionText: String = _
  @BeanProperty var createdBy: String = _
  @BeanProperty var description: String = _
  @BeanProperty var createdAt: Date = _
  @BeanProperty var voteWeightedAverage: Float = _
  @BeanProperty var voteAverage: Float = _
  @BeanProperty var word: String = _
  override def toString: String = {
    val sb = new StringBuilder
    sb.append("class AudioFile {\n")
    sb.append("  attributionUrl: ").append(attributionUrl).append("\n")
    sb.append("  commentCount: ").append(commentCount).append("\n")
    sb.append("  voteCount: ").append(voteCount).append("\n")
    sb.append("  fileUrl: ").append(fileUrl).append("\n")
    sb.append("  audioType: ").append(audioType).append("\n")
    sb.append("  id: ").append(id).append("\n")
    sb.append("  duration: ").append(duration).append("\n")
    sb.append("  attributionText: ").append(attributionText).append("\n")
    sb.append("  createdBy: ").append(createdBy).append("\n")
    sb.append("  description: ").append(description).append("\n")
    sb.append("  createdAt: ").append(createdAt).append("\n")
    sb.append("  voteWeightedAverage: ").append(voteWeightedAverage).append("\n")
    sb.append("  voteAverage: ").append(voteAverage).append("\n")
    sb.append("  word: ").append(word).append("\n")
    sb.append("}\n")
    sb.toString
  }
}
