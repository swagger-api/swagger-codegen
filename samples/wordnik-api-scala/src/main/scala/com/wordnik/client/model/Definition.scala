package com.wordnik.client.model

import scala.reflect.BeanProperty

class Definition {
  @BeanProperty var text: String = _
  @BeanProperty var source: String = _
  @BeanProperty var note: String = _
  @BeanProperty var partOfSpeech: String = _
  @BeanProperty var partOfSpeechObj: PartOfSpeech = _
  override def toString: String = {
    val sb = new StringBuilder
    sb.append("class Definition {\n")
    sb.append("  text: ").append(text).append("\n")
    sb.append("  source: ").append(source).append("\n")
    sb.append("  note: ").append(note).append("\n")
    sb.append("  partOfSpeech: ").append(partOfSpeech).append("\n")
    sb.append("  partOfSpeechObj: ").append(partOfSpeechObj).append("\n")
    sb.append("}\n")
    sb.toString
  }
}
