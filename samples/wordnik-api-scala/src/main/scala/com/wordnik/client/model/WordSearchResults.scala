package com.wordnik.client.model

import scala.reflect.BeanProperty

class WordSearchResults {
  @BeanProperty var totalResults: Int = 0
  @BeanProperty var searchResults: java.util.List[WordSearchResult] = new java.util.ArrayList[WordSearchResult]()
  override def toString: String = {
    val sb = new StringBuilder
    sb.append("class WordSearchResults {\n")
    sb.append("  totalResults: ").append(totalResults).append("\n")
    sb.append("  searchResults: ").append(searchResults).append("\n")
    sb.append("}\n")
    sb.toString
  }
}
