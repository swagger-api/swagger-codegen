package com.wordnik.client.model;

public class ApiTokenStatus {
  private boolean valid = false;
  private String token = null;
  private long resetsInMillis = 0L;
  private long remainingCalls = 0L;
  private long expiresInMillis = 0L;
  private long totalRequests = 0L;
  public boolean getValid() {
    return valid;
  }
  public void setValid(boolean valid) {
    this.valid = valid;
  }

  public String getToken() {
    return token;
  }
  public void setToken(String token) {
    this.token = token;
  }

  public long getResetsInMillis() {
    return resetsInMillis;
  }
  public void setResetsInMillis(long resetsInMillis) {
    this.resetsInMillis = resetsInMillis;
  }

  public long getRemainingCalls() {
    return remainingCalls;
  }
  public void setRemainingCalls(long remainingCalls) {
    this.remainingCalls = remainingCalls;
  }

  public long getExpiresInMillis() {
    return expiresInMillis;
  }
  public void setExpiresInMillis(long expiresInMillis) {
    this.expiresInMillis = expiresInMillis;
  }

  public long getTotalRequests() {
    return totalRequests;
  }
  public void setTotalRequests(long totalRequests) {
    this.totalRequests = totalRequests;
  }

  @Override
  public String toString()  {
    StringBuilder sb = new StringBuilder();
    sb.append("class ApiTokenStatus {\n");
    sb.append("  valid: ").append(valid).append("\n");
    sb.append("  token: ").append(token).append("\n");
    sb.append("  resetsInMillis: ").append(resetsInMillis).append("\n");
    sb.append("  remainingCalls: ").append(remainingCalls).append("\n");
    sb.append("  expiresInMillis: ").append(expiresInMillis).append("\n");
    sb.append("  totalRequests: ").append(totalRequests).append("\n");
    sb.append("}\n");
    return sb.toString();
  }
}
