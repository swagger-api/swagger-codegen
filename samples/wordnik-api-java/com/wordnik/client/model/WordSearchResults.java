package com.wordnik.client.model;

import java.util.*;
public class WordSearchResults {
  private int totalResults = 0;
  private List<WordSearchResult> searchResults = new ArrayList<WordSearchResult>();
  public int getTotalResults() {
    return totalResults;
  }
  public void setTotalResults(int totalResults) {
    this.totalResults = totalResults;
  }

  public List<WordSearchResult> getSearchResults() {
    return searchResults;
  }
  public void setSearchResults(List<WordSearchResult> searchResults) {
    this.searchResults = searchResults;
  }

  @Override
  public String toString()  {
    StringBuilder sb = new StringBuilder();
    sb.append("class WordSearchResults {\n");
    sb.append("  totalResults: ").append(totalResults).append("\n");
    sb.append("  searchResults: ").append(searchResults).append("\n");
    sb.append("}\n");
    return sb.toString();
  }
}
