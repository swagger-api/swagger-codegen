package com.wordnik.client.model

import scala.reflect.BeanProperty

class WordObject {
  @BeanProperty var id: Long = 0L
  @BeanProperty var word: String = _
  @BeanProperty var vulgar: String = _
  override def toString: String = {
    val sb = new StringBuilder
    sb.append("class WordObject {\n")
    sb.append("  id: ").append(id).append("\n")
    sb.append("  word: ").append(word).append("\n")
    sb.append("  vulgar: ").append(vulgar).append("\n")
    sb.append("}\n")
    sb.toString
  }
}
