package com.wordnik.client.model

import scala.reflect.BeanProperty

class AuthenticationToken {
  @BeanProperty var token: String = _
  @BeanProperty var userId: Long = 0L
  @BeanProperty var userSignature: String = _
  override def toString: String = {
    val sb = new StringBuilder
    sb.append("class AuthenticationToken {\n")
    sb.append("  token: ").append(token).append("\n")
    sb.append("  userId: ").append(userId).append("\n")
    sb.append("  userSignature: ").append(userSignature).append("\n")
    sb.append("}\n")
    sb.toString
  }
}
