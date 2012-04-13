package com.wordnik.client.model

import scala.reflect.BeanProperty

class Note {
  @BeanProperty var noteType: String = _
  @BeanProperty var appliesTo: java.util.List[String] = new java.util.ArrayList[String]()
  @BeanProperty var value: String = _
  @BeanProperty var pos: Int = 0
  override def toString: String = {
    val sb = new StringBuilder
    sb.append("class Note {\n")
    sb.append("  noteType: ").append(noteType).append("\n")
    sb.append("  appliesTo: ").append(appliesTo).append("\n")
    sb.append("  value: ").append(value).append("\n")
    sb.append("  pos: ").append(pos).append("\n")
    sb.append("}\n")
    sb.toString
  }
}
