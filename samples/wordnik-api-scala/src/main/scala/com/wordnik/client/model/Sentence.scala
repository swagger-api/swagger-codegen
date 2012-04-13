package com.wordnik.client.model

import scala.reflect.BeanProperty

class Sentence {
  @BeanProperty var id: Long = 0L
  @BeanProperty var hasScoredWords: Boolean = _
  @BeanProperty var scoredWords: java.util.List[ScoredWord] = new java.util.ArrayList[ScoredWord]()
  @BeanProperty var display: String = _
  @BeanProperty var rating: Int = 0
  @BeanProperty var documentMetadataId: Long = 0L
  override def toString: String = {
    val sb = new StringBuilder
    sb.append("class Sentence {\n")
    sb.append("  id: ").append(id).append("\n")
    sb.append("  hasScoredWords: ").append(hasScoredWords).append("\n")
    sb.append("  scoredWords: ").append(scoredWords).append("\n")
    sb.append("  display: ").append(display).append("\n")
    sb.append("  rating: ").append(rating).append("\n")
    sb.append("  documentMetadataId: ").append(documentMetadataId).append("\n")
    sb.append("}\n")
    sb.toString
  }
}
