package com.wordnik.client.model

import scala.reflect.BeanProperty

class FrequencySummary {
  @BeanProperty var unknownYearCount: Int = 0
  @BeanProperty var totalCount: Long = 0L
  @BeanProperty var frequencyString: String = _
  @BeanProperty var word: String = _
  @BeanProperty var frequency: java.util.List[Frequency] = new java.util.ArrayList[Frequency]()
  override def toString: String = {
    val sb = new StringBuilder
    sb.append("class FrequencySummary {\n")
    sb.append("  unknownYearCount: ").append(unknownYearCount).append("\n")
    sb.append("  totalCount: ").append(totalCount).append("\n")
    sb.append("  frequencyString: ").append(frequencyString).append("\n")
    sb.append("  word: ").append(word).append("\n")
    sb.append("  frequency: ").append(frequency).append("\n")
    sb.append("}\n")
    sb.toString
  }
}
