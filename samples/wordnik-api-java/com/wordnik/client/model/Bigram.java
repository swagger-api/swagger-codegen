package com.wordnik.client.model;

public class Bigram {
  private long count = 0L;
  private String gram2 = null;
  private String gram1 = null;
  private double wlmi = 0.0;
  private double mi = 0.0;
  public long getCount() {
    return count;
  }
  public void setCount(long count) {
    this.count = count;
  }

  public String getGram2() {
    return gram2;
  }
  public void setGram2(String gram2) {
    this.gram2 = gram2;
  }

  public String getGram1() {
    return gram1;
  }
  public void setGram1(String gram1) {
    this.gram1 = gram1;
  }

  public double getWlmi() {
    return wlmi;
  }
  public void setWlmi(double wlmi) {
    this.wlmi = wlmi;
  }

  public double getMi() {
    return mi;
  }
  public void setMi(double mi) {
    this.mi = mi;
  }

  @Override
  public String toString()  {
    StringBuilder sb = new StringBuilder();
    sb.append("class Bigram {\n");
    sb.append("  count: ").append(count).append("\n");
    sb.append("  gram2: ").append(gram2).append("\n");
    sb.append("  gram1: ").append(gram1).append("\n");
    sb.append("  wlmi: ").append(wlmi).append("\n");
    sb.append("  mi: ").append(mi).append("\n");
    sb.append("}\n");
    return sb.toString();
  }
}
