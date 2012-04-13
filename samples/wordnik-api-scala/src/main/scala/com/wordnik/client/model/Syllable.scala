package com.wordnik.client.model

import scala.reflect.BeanProperty

class Syllable {
  @BeanProperty var text: String = _
  @BeanProperty var seq: Int = 0
  @BeanProperty var `type`: String = _
  override def toString: String = {
    val sb = new StringBuilder
    sb.append("class Syllable {\n")
    sb.append("  text: ").append(text).append("\n")
    sb.append("  seq: ").append(seq).append("\n")
    sb.append("  `type`: ").append(`type`).append("\n")
    sb.append("}\n")
    sb.toString
  }
}
