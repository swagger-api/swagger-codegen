package com.wordnik.client.model

import scala.reflect.BeanProperty

class WordSearchResult {
  @BeanProperty var count: Long = 0L
  @BeanProperty var lexicality: Double = 0.0
  @BeanProperty var word: String = _
  override def toString: String = {
    val sb = new StringBuilder
    sb.append("class WordSearchResult {\n")
    sb.append("  count: ").append(count).append("\n")
    sb.append("  lexicality: ").append(lexicality).append("\n")
    sb.append("  word: ").append(word).append("\n")
    sb.append("}\n")
    sb.toString
  }
}
