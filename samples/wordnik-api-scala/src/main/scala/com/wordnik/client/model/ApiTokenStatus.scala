package com.wordnik.client.model

import scala.reflect.BeanProperty

class ApiTokenStatus {
  @BeanProperty var valid: Boolean = _
  @BeanProperty var token: String = _
  @BeanProperty var resetsInMillis: Long = 0L
  @BeanProperty var remainingCalls: Long = 0L
  @BeanProperty var expiresInMillis: Long = 0L
  @BeanProperty var totalRequests: Long = 0L
  override def toString: String = {
    val sb = new StringBuilder
    sb.append("class ApiTokenStatus {\n")
    sb.append("  valid: ").append(valid).append("\n")
    sb.append("  token: ").append(token).append("\n")
    sb.append("  resetsInMillis: ").append(resetsInMillis).append("\n")
    sb.append("  remainingCalls: ").append(remainingCalls).append("\n")
    sb.append("  expiresInMillis: ").append(expiresInMillis).append("\n")
    sb.append("  totalRequests: ").append(totalRequests).append("\n")
    sb.append("}\n")
    sb.toString
  }
}
