package com.wordnik.client.model

import scala.reflect.BeanProperty

class Citation {
  @BeanProperty var cite: String = _
  @BeanProperty var source: String = _
  override def toString: String = {
    val sb = new StringBuilder
    sb.append("class Citation {\n")
    sb.append("  cite: ").append(cite).append("\n")
    sb.append("  source: ").append(source).append("\n")
    sb.append("}\n")
    sb.toString
  }
}
