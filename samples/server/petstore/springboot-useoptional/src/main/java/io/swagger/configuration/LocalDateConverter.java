package io.swagger.configuration;

import org.springframework.core.convert.converter.Converter;
import org.threeten.bp.LocalDate;
import org.threeten.bp.format.DateTimeFormatter;

public class LocalDateConverter implements Converter<String, LocalDate> {
    private final DateTimeFormatter formatter;

    public LocalDateConverter(String dateFormat) {
        this.formatter = DateTimeFormatter.ofPattern(dateFormat);
    }

    @Override
    public LocalDate convert(String source) {
        if(source == null || source.isEmpty()) {
            return null;
        }
        return LocalDate.parse(source, this.formatter);
    }
}
