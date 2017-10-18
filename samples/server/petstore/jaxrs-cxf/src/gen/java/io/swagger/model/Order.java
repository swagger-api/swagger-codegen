package io.swagger.model;

import io.swagger.annotations.ApiModelProperty;

import javax.xml.bind.annotation.XmlEnum;
import javax.xml.bind.annotation.XmlEnumValue;
import javax.xml.bind.annotation.XmlType;
import java.util.Date;

public class Order  {
  
  @ApiModelProperty(value = "")
  private Long id = null;
  @ApiModelProperty(value = "")
  private Long petId = null;
  @ApiModelProperty(value = "")
  private Integer quantity = null;
  @ApiModelProperty(value = "")
  private Date shipDate = null;

@XmlType(name="StatusEnum")
@XmlEnum(String.class)
public enum StatusEnum {

@XmlEnumValue("placed") PLACED(String.valueOf("placed")), @XmlEnumValue("approved") APPROVED(String.valueOf("approved")), @XmlEnumValue("delivered") DELIVERED(String.valueOf("delivered"));


    private String value;

    StatusEnum (String v) {
        value = v;
    }

    public String value() {
        return value;
    }

    @Override
    public String toString() {
        return String.valueOf(value);
    }

    public static StatusEnum fromValue(String v) {
        for (StatusEnum b : StatusEnum.values()) {
            if (String.valueOf(b.value).equals(v)) {
                return b;
            }
        }
        return null;
    }
}

  @ApiModelProperty(value = "Order Status")
  private StatusEnum status = null;
  @ApiModelProperty(value = "")
  private Boolean complete = false;

  public Order() {}


  public Order(Long id, Long petId, Integer quantity, Date shipDate, StatusEnum status, Boolean complete) {
    this.id = id;
    this.petId = petId;
    this.quantity = quantity;
    this.shipDate = shipDate;
    this.status = status;
    this.complete = complete;
  }

 /**
   * Get id
   * @return id
  **/
  public Long getId() {
    return id;
  }

  public void setId(Long id) {
    this.id = id;
  }

  public Order id(Long id) {
    this.id = id;
    return this;
  }

 /**
   * Get petId
   * @return petId
  **/
  public Long getPetId() {
    return petId;
  }

  public void setPetId(Long petId) {
    this.petId = petId;
  }

  public Order petId(Long petId) {
    this.petId = petId;
    return this;
  }

 /**
   * Get quantity
   * @return quantity
  **/
  public Integer getQuantity() {
    return quantity;
  }

  public void setQuantity(Integer quantity) {
    this.quantity = quantity;
  }

  public Order quantity(Integer quantity) {
    this.quantity = quantity;
    return this;
  }

 /**
   * Get shipDate
   * @return shipDate
  **/
  public Date getShipDate() {
    return shipDate;
  }

  public void setShipDate(Date shipDate) {
    this.shipDate = shipDate;
  }

  public Order shipDate(Date shipDate) {
    this.shipDate = shipDate;
    return this;
  }

 /**
   * Order Status
   * @return status
  **/
  public StatusEnum getStatus() {
    return status;
  }

  public void setStatus(StatusEnum status) {
    this.status = status;
  }

  public Order status(StatusEnum status) {
    this.status = status;
    return this;
  }

 /**
   * Get complete
   * @return complete
  **/
  public Boolean isComplete() {
    return complete;
  }

  public void setComplete(Boolean complete) {
    this.complete = complete;
  }

  public Order complete(Boolean complete) {
    this.complete = complete;
    return this;
  }


  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class Order {\n");
    
    sb.append("    id: ").append(toIndentedString(id)).append("\n");
    sb.append("    petId: ").append(toIndentedString(petId)).append("\n");
    sb.append("    quantity: ").append(toIndentedString(quantity)).append("\n");
    sb.append("    shipDate: ").append(toIndentedString(shipDate)).append("\n");
    sb.append("    status: ").append(toIndentedString(status)).append("\n");
    sb.append("    complete: ").append(toIndentedString(complete)).append("\n");
    sb.append("}");
    return sb.toString();
  }

  /**
   * Convert the given object to string with each line indented by 4 spaces
   * (except the first line).
   */
  private static String toIndentedString(java.lang.Object o) {
    if (o == null) {
      return "null";
    }
    return o.toString().replace("\n", "\n    ");
  }
}

