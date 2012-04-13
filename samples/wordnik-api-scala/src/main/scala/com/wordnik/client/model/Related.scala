package com.wordnik.client.model

import scala.reflect.BeanProperty

class Related {
  @BeanProperty var label1: String = _
  @BeanProperty var label2: String = _
  @BeanProperty var relationshipType: String = _
  @BeanProperty var label3: String = _
  @BeanProperty var words: java.util.List[String] = new java.util.ArrayList[String]()
  @BeanProperty var label4: String = _
  @BeanProperty var gram: String = _
  override def toString: String = {
    val sb = new StringBuilder
    sb.append("class Related {\n")
    sb.append("  label1: ").append(label1).append("\n")
    sb.append("  label2: ").append(label2).append("\n")
    sb.append("  relationshipType: ").append(relationshipType).append("\n")
    sb.append("  label3: ").append(label3).append("\n")
    sb.append("  words: ").append(words).append("\n")
    sb.append("  label4: ").append(label4).append("\n")
    sb.append("  gram: ").append(gram).append("\n")
    sb.append("}\n")
    sb.toString
  }
}
