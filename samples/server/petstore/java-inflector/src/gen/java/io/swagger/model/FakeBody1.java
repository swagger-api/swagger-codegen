package io.swagger.model;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import io.swagger.v3.oas.annotations.media.Schema;
import java.io.File;
import java.math.BigDecimal;
import java.util.Date;




public class FakeBody1   {
  @JsonProperty("integer")
  private Integer integer = null;
  @JsonProperty("int32")
  private Integer int32 = null;
  @JsonProperty("int64")
  private Long int64 = null;
  @JsonProperty("number")
  private BigDecimal number = null;
  @JsonProperty("float")
  private Float _float = null;
  @JsonProperty("double")
  private Double _double = null;
  @JsonProperty("string")
  private String string = null;
  @JsonProperty("pattern_without_delimiter")
  private String patternWithoutDelimiter = null;
  @JsonProperty("byte")
  private byte[] _byte = null;
  @JsonProperty("binary")
  private File binary = null;
  @JsonProperty("date")
  private Date date = null;
  @JsonProperty("dateTime")
  private Date dateTime = null;
  @JsonProperty("password")
  private String password = null;
  @JsonProperty("callback")
  private String callback = null;
  /**
   * None
   * minimum: 10
   * maximum: 100
   **/
  public FakeBody1 integer(Integer integer) {
    this.integer = integer;
    return this;
  }

  
  @Schema(description = "None")
  @JsonProperty("integer")
  public Integer getInteger() {
    return integer;
  }
  public void setInteger(Integer integer) {
    this.integer = integer;
  }

  /**
   * None
   * minimum: 20
   * maximum: 200
   **/
  public FakeBody1 int32(Integer int32) {
    this.int32 = int32;
    return this;
  }

  
  @Schema(description = "None")
  @JsonProperty("int32")
  public Integer getInt32() {
    return int32;
  }
  public void setInt32(Integer int32) {
    this.int32 = int32;
  }

  /**
   * None
   **/
  public FakeBody1 int64(Long int64) {
    this.int64 = int64;
    return this;
  }

  
  @Schema(description = "None")
  @JsonProperty("int64")
  public Long getInt64() {
    return int64;
  }
  public void setInt64(Long int64) {
    this.int64 = int64;
  }

  /**
   * None
   * minimum: 32
   * maximum: 543
   **/
  public FakeBody1 number(BigDecimal number) {
    this.number = number;
    return this;
  }

  
  @Schema(required = true, description = "None")
  @JsonProperty("number")
  public BigDecimal getNumber() {
    return number;
  }
  public void setNumber(BigDecimal number) {
    this.number = number;
  }

  /**
   * None
   * maximum: 987
   **/
  public FakeBody1 _float(Float _float) {
    this._float = _float;
    return this;
  }

  
  @Schema(description = "None")
  @JsonProperty("float")
  public Float getFloat() {
    return _float;
  }
  public void setFloat(Float _float) {
    this._float = _float;
  }

  /**
   * None
   * minimum: 67
   * maximum: 123
   **/
  public FakeBody1 _double(Double _double) {
    this._double = _double;
    return this;
  }

  
  @Schema(required = true, description = "None")
  @JsonProperty("double")
  public Double getDouble() {
    return _double;
  }
  public void setDouble(Double _double) {
    this._double = _double;
  }

  /**
   * None
   **/
  public FakeBody1 string(String string) {
    this.string = string;
    return this;
  }

  
  @Schema(description = "None")
  @JsonProperty("string")
  public String getString() {
    return string;
  }
  public void setString(String string) {
    this.string = string;
  }

  /**
   * None
   **/
  public FakeBody1 patternWithoutDelimiter(String patternWithoutDelimiter) {
    this.patternWithoutDelimiter = patternWithoutDelimiter;
    return this;
  }

  
  @Schema(required = true, description = "None")
  @JsonProperty("pattern_without_delimiter")
  public String getPatternWithoutDelimiter() {
    return patternWithoutDelimiter;
  }
  public void setPatternWithoutDelimiter(String patternWithoutDelimiter) {
    this.patternWithoutDelimiter = patternWithoutDelimiter;
  }

  /**
   * None
   **/
  public FakeBody1 _byte(byte[] _byte) {
    this._byte = _byte;
    return this;
  }

  
  @Schema(required = true, description = "None")
  @JsonProperty("byte")
  public byte[] getByte() {
    return _byte;
  }
  public void setByte(byte[] _byte) {
    this._byte = _byte;
  }

  /**
   * None
   **/
  public FakeBody1 binary(File binary) {
    this.binary = binary;
    return this;
  }

  
  @Schema(description = "None")
  @JsonProperty("binary")
  public File getBinary() {
    return binary;
  }
  public void setBinary(File binary) {
    this.binary = binary;
  }

  /**
   * None
   **/
  public FakeBody1 date(Date date) {
    this.date = date;
    return this;
  }

  
  @Schema(description = "None")
  @JsonProperty("date")
  public Date getDate() {
    return date;
  }
  public void setDate(Date date) {
    this.date = date;
  }

  /**
   * None
   **/
  public FakeBody1 dateTime(Date dateTime) {
    this.dateTime = dateTime;
    return this;
  }

  
  @Schema(description = "None")
  @JsonProperty("dateTime")
  public Date getDateTime() {
    return dateTime;
  }
  public void setDateTime(Date dateTime) {
    this.dateTime = dateTime;
  }

  /**
   * None
   **/
  public FakeBody1 password(String password) {
    this.password = password;
    return this;
  }

  
  @Schema(description = "None")
  @JsonProperty("password")
  public String getPassword() {
    return password;
  }
  public void setPassword(String password) {
    this.password = password;
  }

  /**
   * None
   **/
  public FakeBody1 callback(String callback) {
    this.callback = callback;
    return this;
  }

  
  @Schema(description = "None")
  @JsonProperty("callback")
  public String getCallback() {
    return callback;
  }
  public void setCallback(String callback) {
    this.callback = callback;
  }


  @Override
  public boolean equals(java.lang.Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    FakeBody1 fakeBody1 = (FakeBody1) o;
    return Objects.equals(integer, fakeBody1.integer) &&
        Objects.equals(int32, fakeBody1.int32) &&
        Objects.equals(int64, fakeBody1.int64) &&
        Objects.equals(number, fakeBody1.number) &&
        Objects.equals(_float, fakeBody1._float) &&
        Objects.equals(_double, fakeBody1._double) &&
        Objects.equals(string, fakeBody1.string) &&
        Objects.equals(patternWithoutDelimiter, fakeBody1.patternWithoutDelimiter) &&
        Objects.equals(_byte, fakeBody1._byte) &&
        Objects.equals(binary, fakeBody1.binary) &&
        Objects.equals(date, fakeBody1.date) &&
        Objects.equals(dateTime, fakeBody1.dateTime) &&
        Objects.equals(password, fakeBody1.password) &&
        Objects.equals(callback, fakeBody1.callback);
  }

  @Override
  public int hashCode() {
    return Objects.hash(integer, int32, int64, number, _float, _double, string, patternWithoutDelimiter, _byte, binary, date, dateTime, password, callback);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class FakeBody1 {\n");
    sb.append("    integer: ").append(toIndentedString(integer)).append("\n");
    sb.append("    int32: ").append(toIndentedString(int32)).append("\n");
    sb.append("    int64: ").append(toIndentedString(int64)).append("\n");
    sb.append("    number: ").append(toIndentedString(number)).append("\n");
    sb.append("    _float: ").append(toIndentedString(_float)).append("\n");
    sb.append("    _double: ").append(toIndentedString(_double)).append("\n");
    sb.append("    string: ").append(toIndentedString(string)).append("\n");
    sb.append("    patternWithoutDelimiter: ").append(toIndentedString(patternWithoutDelimiter)).append("\n");
    sb.append("    _byte: ").append(toIndentedString(_byte)).append("\n");
    sb.append("    binary: ").append(toIndentedString(binary)).append("\n");
    sb.append("    date: ").append(toIndentedString(date)).append("\n");
    sb.append("    dateTime: ").append(toIndentedString(dateTime)).append("\n");
    sb.append("    password: ").append(toIndentedString(password)).append("\n");
    sb.append("    callback: ").append(toIndentedString(callback)).append("\n");
    sb.append("}");
    return sb.toString();
  }

  /**
   * Convert the given object to string with each line indented by 4 spaces
   * (except the first line).
   */
  private String toIndentedString(java.lang.Object o) {
    if (o == null) {
      return "null";
    }
    return o.toString().replace("\n", "\n    ");
  }
}
