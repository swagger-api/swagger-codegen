package com.wordnik.client.model

import scala.reflect.BeanProperty

class Facet {
  @BeanProperty var facetValues: java.util.List[FacetValue] = new java.util.ArrayList[FacetValue]()
  @BeanProperty var name: String = _
  override def toString: String = {
    val sb = new StringBuilder
    sb.append("class Facet {\n")
    sb.append("  facetValues: ").append(facetValues).append("\n")
    sb.append("  name: ").append(name).append("\n")
    sb.append("}\n")
    sb.toString
  }
}
