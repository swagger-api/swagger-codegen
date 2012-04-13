package com.wordnik.client.model

import scala.reflect.BeanProperty

class FacetValue {
  @BeanProperty var value: String = _
  @BeanProperty var count: Long = 0L

  override def toString: String = {
    val sb = new StringBuilder
    sb.append("class FacetValue {\n")
    sb.append("  value: ").append(value).append("\n")
    sb.append("  count: ").append(count).append("\n")
    sb.append("}\n")
    sb.toString
  }
}

