package com.wordnik.client.model

import scala.reflect.BeanProperty

class Bigram {
  @BeanProperty var count: Long = 0L
  @BeanProperty var gram2: String = _
  @BeanProperty var gram1: String = _
  @BeanProperty var wlmi: Double = 0.0
  @BeanProperty var mi: Double = 0.0
  override def toString: String = {
    val sb = new StringBuilder
    sb.append("class Bigram {\n")
    sb.append("  count: ").append(count).append("\n")
    sb.append("  gram2: ").append(gram2).append("\n")
    sb.append("  gram1: ").append(gram1).append("\n")
    sb.append("  wlmi: ").append(wlmi).append("\n")
    sb.append("  mi: ").append(mi).append("\n")
    sb.append("}\n")
    sb.toString
  }
}
