package com.wordnik.client.model

import scala.reflect.BeanProperty

class ContentProvider {
  @BeanProperty var id: Int = 0
  @BeanProperty var name: String = _
  override def toString: String = {
    val sb = new StringBuilder
    sb.append("class ContentProvider {\n")
    sb.append("  id: ").append(id).append("\n")
    sb.append("  name: ").append(name).append("\n")
    sb.append("}\n")
    sb.toString
  }
}
