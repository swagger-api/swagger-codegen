package com.wordnik.client.model

import scala.reflect.BeanProperty

class ExampleSearchResults {
  @BeanProperty var facets: java.util.List[Facet] = new java.util.ArrayList[Facet]()
  @BeanProperty var examples: java.util.List[Example] = new java.util.ArrayList[Example]()
  override def toString: String = {
    val sb = new StringBuilder
    sb.append("class ExampleSearchResults {\n")
    sb.append("  facets: ").append(facets).append("\n")
    sb.append("  examples: ").append(examples).append("\n")
    sb.append("}\n")
    sb.toString
  }
}
