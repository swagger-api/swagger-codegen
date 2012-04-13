package com.wordnik.client.model

import java.util.Date
import scala.reflect.BeanProperty

class WordListWord {
  @BeanProperty var id: Long = 0L
  @BeanProperty var username: String = _
  @BeanProperty var createdAt: Date = _
  @BeanProperty var userId: Long = 0L
  @BeanProperty var numberCommentsOnWord: Long = 0L
  @BeanProperty var word: String = _
  @BeanProperty var numberLists: Long = 0L
  override def toString: String = {
    val sb = new StringBuilder
    sb.append("class WordListWord {\n")
    sb.append("  id: ").append(id).append("\n")
    sb.append("  username: ").append(username).append("\n")
    sb.append("  createdAt: ").append(createdAt).append("\n")
    sb.append("  userId: ").append(userId).append("\n")
    sb.append("  numberCommentsOnWord: ").append(numberCommentsOnWord).append("\n")
    sb.append("  word: ").append(word).append("\n")
    sb.append("  numberLists: ").append(numberLists).append("\n")
    sb.append("}\n")
    sb.toString
  }
}
