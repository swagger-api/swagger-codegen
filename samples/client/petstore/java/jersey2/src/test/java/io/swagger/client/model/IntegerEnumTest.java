package io.swagger.client.model;

import org.junit.Assert;
import org.junit.Test;

public class IntegerEnumTest {

    @Test
    public void fromValue() {
        for (final IntegerEnum enumeration : IntegerEnum.values()) {
            final Integer value = enumeration.getValue();
            Assert.assertNotNull(value);
            Assert.assertEquals(IntegerEnum.fromValue(value.toString()), enumeration);
        }
    }
}
