package io.swagger.client.model;

import org.junit.Assert;
import org.junit.Test;

public class LongEnumTest {

    @Test
    public void fromValue() {
        for (final LongEnum enumeration : LongEnum.values()) {
            final Long value = enumeration.getValue();
            Assert.assertNotNull(value);
            Assert.assertEquals(LongEnum.fromValue(value.toString()), enumeration);
        }
    }
}
