package com.wordnik.client.model

import scala.reflect.BeanProperty

class User {
  @BeanProperty var id: Long = 0L
  @BeanProperty var username: String = _
  @BeanProperty var status: Int = 0
  @BeanProperty var email: String = _
  @BeanProperty var faceBookId: String = _
  @BeanProperty var userName: String = _
  @BeanProperty var displayName: String = _
  @BeanProperty var password: String = _
  override def toString: String = {
    val sb = new StringBuilder
    sb.append("class User {\n")
    sb.append("  id: ").append(id).append("\n")
    sb.append("  username: ").append(username).append("\n")
    sb.append("  status: ").append(status).append("\n")
    sb.append("  email: ").append(email).append("\n")
    sb.append("  faceBookId: ").append(faceBookId).append("\n")
    sb.append("  userName: ").append(userName).append("\n")
    sb.append("  displayName: ").append(displayName).append("\n")
    sb.append("  password: ").append(password).append("\n")
    sb.append("}\n")
    sb.toString
  }
}
