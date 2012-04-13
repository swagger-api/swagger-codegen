package com.wordnik.client.model

import scala.reflect.BeanProperty

class Example {
  @BeanProperty var id: Long = 0L
  @BeanProperty var text: String = _
  @BeanProperty var title: String = _
  @BeanProperty var url: String = _
  override def toString: String = {
    val sb = new StringBuilder
    sb.append("class Example {\n")
    sb.append("  id: ").append(id).append("\n")
    sb.append("  text: ").append(text).append("\n")
    sb.append("  title: ").append(title).append("\n")
    sb.append("  url: ").append(url).append("\n")
    sb.append("}\n")
    sb.toString
  }
}
