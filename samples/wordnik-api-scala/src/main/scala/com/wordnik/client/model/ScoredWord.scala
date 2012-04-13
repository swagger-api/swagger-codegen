package com.wordnik.client.model

import scala.reflect.BeanProperty

class ScoredWord {
  @BeanProperty var id: Long = 0L
  @BeanProperty var position: Int = 0
  @BeanProperty var lemma: String = _
  @BeanProperty var docTermCount: Int = 0
  @BeanProperty var wordType: String = _
  @BeanProperty var score: Float = _
  @BeanProperty var word: String = _
  @BeanProperty var sentenceId: Long = 0L
  @BeanProperty var stopword: Boolean = _
  @BeanProperty var baseWordScore: Double = 0.0
  @BeanProperty var partOfSpeech: String = _
  override def toString: String = {
    val sb = new StringBuilder
    sb.append("class ScoredWord {\n")
    sb.append("  id: ").append(id).append("\n")
    sb.append("  position: ").append(position).append("\n")
    sb.append("  lemma: ").append(lemma).append("\n")
    sb.append("  docTermCount: ").append(docTermCount).append("\n")
    sb.append("  wordType: ").append(wordType).append("\n")
    sb.append("  score: ").append(score).append("\n")
    sb.append("  word: ").append(word).append("\n")
    sb.append("  sentenceId: ").append(sentenceId).append("\n")
    sb.append("  stopword: ").append(stopword).append("\n")
    sb.append("  baseWordScore: ").append(baseWordScore).append("\n")
    sb.append("  partOfSpeech: ").append(partOfSpeech).append("\n")
    sb.append("}\n")
    sb.toString
  }
}
