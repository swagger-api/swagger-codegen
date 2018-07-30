@XmlJavaTypeAdapters({
  @XmlJavaTypeAdapter(value = OffsetDateTimeAdapter.class, type = OffsetDateTime.class)
})
package io.swagger.model;

import io.swagger.adapters.OffsetDateTimeAdapter;
import org.threeten.bp.OffsetDateTime;

import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapters;