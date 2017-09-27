package io.swagger.client.model;

import org.junit.Assert;
import org.junit.Test;

public class DoubleEnumTest {

    @Test
    public void fromValue() {
        for (final DoubleEnum enumeration : DoubleEnum.values()) {
            final Double value = enumeration.getValue();
            Assert.assertNotNull(value);
            Assert.assertEquals(DoubleEnum.fromValue(value.toString()), enumeration);
        }
    }
}
