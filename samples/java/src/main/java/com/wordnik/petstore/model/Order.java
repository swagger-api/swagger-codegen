package com.wordnik.petstore.model;

import java.util.Date;
public class Order {
  private long id = 0L;
  private long petId = 0L;
  /* Order Status */
  private String status = null;
  private int quantity = 0;
  private Date shipDate = null;
  public long getId() {
    return id;
  }
  public void setId(long id) {
    this.id = id;
  }

  public long getPetId() {
    return petId;
  }
  public void setPetId(long petId) {
    this.petId = petId;
  }

  public String getStatus() {
    return status;
  }
  public void setStatus(String status) {
    this.status = status;
  }

  public int getQuantity() {
    return quantity;
  }
  public void setQuantity(int quantity) {
    this.quantity = quantity;
  }

  public Date getShipDate() {
    return shipDate;
  }
  public void setShipDate(Date shipDate) {
    this.shipDate = shipDate;
  }

  @Override
  public String toString()  {
    StringBuilder sb = new StringBuilder();
    sb.append("class Order {\n");
    sb.append("  id: ").append(id).append("\n");
    sb.append("  petId: ").append(petId).append("\n");
    sb.append("  status: ").append(status).append("\n");
    sb.append("  quantity: ").append(quantity).append("\n");
    sb.append("  shipDate: ").append(shipDate).append("\n");
    sb.append("}\n");
    return sb.toString();
  }
}
