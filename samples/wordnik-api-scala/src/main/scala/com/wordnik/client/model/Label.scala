package com.wordnik.client.model

import scala.reflect.BeanProperty

class Label {
  @BeanProperty var text: String = _
  @BeanProperty var `type`: String = _
  override def toString: String = {
    val sb = new StringBuilder
    sb.append("class Label {\n")
    sb.append("  text: ").append(text).append("\n")
    sb.append("  `type`: ").append(`type`).append("\n")
    sb.append("}\n")
    sb.toString
  }
}
