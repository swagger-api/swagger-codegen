package com.wordnik.client.model

import java.util.Date
import scala.reflect.BeanProperty

class WordList {
  @BeanProperty var id: Long = 0L
  @BeanProperty var updatedAt: Date = _
  @BeanProperty var username: String = _
  @BeanProperty var permalink: String = _
  @BeanProperty var description: String = _
  @BeanProperty var createdAt: Date = _
  @BeanProperty var lastActivityAt: Date = _
  @BeanProperty var name: String = _
  @BeanProperty var userId: Long = 0L
  @BeanProperty var numberWordsInList: Long = 0L
  @BeanProperty var `type`: String = _
  override def toString: String = {
    val sb = new StringBuilder
    sb.append("class WordList {\n")
    sb.append("  id: ").append(id).append("\n")
    sb.append("  updatedAt: ").append(updatedAt).append("\n")
    sb.append("  username: ").append(username).append("\n")
    sb.append("  permalink: ").append(permalink).append("\n")
    sb.append("  description: ").append(description).append("\n")
    sb.append("  createdAt: ").append(createdAt).append("\n")
    sb.append("  lastActivityAt: ").append(lastActivityAt).append("\n")
    sb.append("  name: ").append(name).append("\n")
    sb.append("  userId: ").append(userId).append("\n")
    sb.append("  numberWordsInList: ").append(numberWordsInList).append("\n")
    sb.append("  `type`: ").append(`type`).append("\n")
    sb.append("}\n")
    sb.toString
  }
}
