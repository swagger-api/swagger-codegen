package com.wordnik.client.model

import scala.reflect.BeanProperty

class ExampleUsage {
  @BeanProperty var text: String = _
  override def toString: String = {
    val sb = new StringBuilder
    sb.append("class ExampleUsage {\n")
    sb.append("  text: ").append(text).append("\n")
    sb.append("}\n")
    sb.toString
  }
}
