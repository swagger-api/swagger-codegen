package com.wordnik.client.model;

public class Frequency {
  private long count = 0L;
  private int year = 0;
  public long getCount() {
    return count;
  }
  public void setCount(long count) {
    this.count = count;
  }

  public int getYear() {
    return year;
  }
  public void setYear(int year) {
    this.year = year;
  }

  @Override
  public String toString()  {
    StringBuilder sb = new StringBuilder();
    sb.append("class Frequency {\n");
    sb.append("  count: ").append(count).append("\n");
    sb.append("  year: ").append(year).append("\n");
    sb.append("}\n");
    return sb.toString();
  }
}
