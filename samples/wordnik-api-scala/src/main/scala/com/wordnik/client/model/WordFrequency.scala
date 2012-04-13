package com.wordnik.client.model

import scala.reflect.BeanProperty

class WordFrequency {
  @BeanProperty var count: Long = 0L
  @BeanProperty var wordstring: String = _
  override def toString: String = {
    val sb = new StringBuilder
    sb.append("class WordFrequency {\n")
    sb.append("  count: ").append(count).append("\n")
    sb.append("  wordstring: ").append(wordstring).append("\n")
    sb.append("}\n")
    sb.toString
  }
}
