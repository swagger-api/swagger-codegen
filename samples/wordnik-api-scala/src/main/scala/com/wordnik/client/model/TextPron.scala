package com.wordnik.client.model

import scala.reflect.BeanProperty

class TextPron {
  @BeanProperty var raw: String = _
  @BeanProperty var seq: Int = 0
  @BeanProperty var rawType: String = _
  override def toString: String = {
    val sb = new StringBuilder
    sb.append("class TextPron {\n")
    sb.append("  raw: ").append(raw).append("\n")
    sb.append("  seq: ").append(seq).append("\n")
    sb.append("  rawType: ").append(rawType).append("\n")
    sb.append("}\n")
    sb.toString
  }
}
