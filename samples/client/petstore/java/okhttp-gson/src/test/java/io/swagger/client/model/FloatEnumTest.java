package io.swagger.client.model;

import org.junit.Assert;
import org.junit.Test;

public class FloatEnumTest {

    @Test
    public void fromValue() {
        for (final FloatEnum enumeration : FloatEnum.values()) {
            final Float value = enumeration.getValue();
            Assert.assertNotNull(value);
            Assert.assertEquals(FloatEnum.fromValue(value.toString()), enumeration);
        }
    }
}
