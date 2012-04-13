package com.wordnik.client.model

import scala.reflect.BeanProperty

class Frequency {
  @BeanProperty var count: Long = 0L
  @BeanProperty var year: Int = 0
  override def toString: String = {
    val sb = new StringBuilder
    sb.append("class Frequency {\n")
    sb.append("  count: ").append(count).append("\n")
    sb.append("  year: ").append(year).append("\n")
    sb.append("}\n")
    sb.toString
  }
}
