package io.swagger.client.model;

import io.swagger.client.model.Currency;

import io.swagger.annotations.*;
import com.google.gson.annotations.SerializedName;


/**
 * some description 
 **/
@ApiModel(description = "some description ")
public class Amount  {
  
  @SerializedName("value")
  private Double value = null;
  @SerializedName("currency")
  private Currency currency = null;

  /**
   * some description 
   * minimum: 0.01
   * maximum: 1000000000000000
   **/
  @ApiModelProperty(required = true, value = "some description ")
  public Double getValue() {
    return value;
  }
  public void setValue(Double value) {
    this.value = value;
  }

  /**
   **/
  @ApiModelProperty(required = true, value = "")
  public Currency getCurrency() {
    return currency;
  }
  public void setCurrency(Currency currency) {
    this.currency = currency;
  }


  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    Amount amount = (Amount) o;
    return (this.value == null ? amount.value == null : this.value.equals(amount.value)) &&
        (this.currency == null ? amount.currency == null : this.currency.equals(amount.currency));
  }

  @Override
  public int hashCode() {
    int result = 17;
    result = 31 * result + (this.value == null ? 0: this.value.hashCode());
    result = 31 * result + (this.currency == null ? 0: this.currency.hashCode());
    return result;
  }

  @Override
  public String toString()  {
    StringBuilder sb = new StringBuilder();
    sb.append("class Amount {\n");
    
    sb.append("  value: ").append(value).append("\n");
    sb.append("  currency: ").append(currency).append("\n");
    sb.append("}\n");
    return sb.toString();
  }
}
