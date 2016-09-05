package io.swagger.client;

import com.fasterxml.jackson.annotation.*;
import com.fasterxml.jackson.databind.*;
import com.fasterxml.jackson.datatype.jsr310.JavaTimeModule;
import com.fasterxml.jackson.datatype.jsr310.ser.OffsetDateTimeSerializer;

import java.time.OffsetDateTime;
import java.time.ZoneId;
import java.time.format.DateTimeFormatter;

import java.text.DateFormat;

import javax.ws.rs.ext.ContextResolver;


public class JSON implements ContextResolver<ObjectMapper> {

  //Formatter with fixed length and timezone
  public static final DateTimeFormatter ISO_FIXED_FORMAT =
    DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm:ss.SSS'Z'").withZone(ZoneId.of("UTC"));

  private ObjectMapper mapper;

  public JSON() {
    mapper = new ObjectMapper();
    mapper.setSerializationInclusion(JsonInclude.Include.NON_NULL);
    mapper.configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false);
    mapper.disable(SerializationFeature.WRITE_DATES_AS_TIMESTAMPS);
    mapper.enable(SerializationFeature.WRITE_ENUMS_USING_TO_STRING);
    mapper.enable(DeserializationFeature.READ_ENUMS_USING_TO_STRING);
    mapper.registerModule(new JavaTimeModule()
      .addSerializer(OffsetDateTime.class, new CustomOffsetDateTimeSerializer(ISO_FIXED_FORMAT))
    );
  }

  /**
   * Set the date format for JSON (de)serialization with Date properties.
   */
  public void setDateFormat(DateFormat dateFormat) {
    mapper.setDateFormat(dateFormat);
  }

  @Override
  public ObjectMapper getContext(Class<?> type) {
    return mapper;
  }
}

class CustomOffsetDateTimeSerializer extends OffsetDateTimeSerializer {

  public CustomOffsetDateTimeSerializer(DateTimeFormatter format) {
    super(OffsetDateTimeSerializer.INSTANCE, false, format);
  }
}
