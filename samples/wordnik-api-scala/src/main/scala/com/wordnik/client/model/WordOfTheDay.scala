package com.wordnik.client.model

import java.util.Date
import scala.reflect.BeanProperty

class WordOfTheDay {
  @BeanProperty var id: Long = 0L
  @BeanProperty var parentId: String = _
  @BeanProperty var category: String = _
  @BeanProperty var createdBy: String = _
  @BeanProperty var createdAt: Date = _
  @BeanProperty var contentProvider: ContentProvider = _
  @BeanProperty var word: String = _
  @BeanProperty var htmlExtra: String = _
  @BeanProperty var definitions: java.util.List[Definition] = new java.util.ArrayList[Definition]()
  @BeanProperty var examples: java.util.List[Example] = new java.util.ArrayList[Example]()
  @BeanProperty var publishDate: Date = _
  @BeanProperty var note: String = _
  override def toString: String = {
    val sb = new StringBuilder
    sb.append("class WordOfTheDay {\n")
    sb.append("  id: ").append(id).append("\n")
    sb.append("  parentId: ").append(parentId).append("\n")
    sb.append("  category: ").append(category).append("\n")
    sb.append("  createdBy: ").append(createdBy).append("\n")
    sb.append("  createdAt: ").append(createdAt).append("\n")
    sb.append("  contentProvider: ").append(contentProvider).append("\n")
    sb.append("  word: ").append(word).append("\n")
    sb.append("  htmlExtra: ").append(htmlExtra).append("\n")
    sb.append("  definitions: ").append(definitions).append("\n")
    sb.append("  examples: ").append(examples).append("\n")
    sb.append("  publishDate: ").append(publishDate).append("\n")
    sb.append("  note: ").append(note).append("\n")
    sb.append("}\n")
    sb.toString
  }
}
