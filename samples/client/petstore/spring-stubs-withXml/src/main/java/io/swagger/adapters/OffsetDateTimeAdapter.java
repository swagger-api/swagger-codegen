package io.swagger.adapters;

import org.threeten.bp.OffsetDateTime;
import org.threeten.bp.format.DateTimeFormatter;

import javax.xml.bind.annotation.adapters.XmlAdapter;

public class OffsetDateTimeAdapter extends XmlAdapter<String, OffsetDateTime>{

  @Override
  public OffsetDateTime unmarshal(String v) throws Exception {

    return OffsetDateTime.parse(v, DateTimeFormatter.ISO_LOCAL_DATE_TIME);
  }

  @Override
  public String marshal(OffsetDateTime v) throws Exception {

    return v.format(DateTimeFormatter.ISO_LOCAL_DATE_TIME);
  }
}
