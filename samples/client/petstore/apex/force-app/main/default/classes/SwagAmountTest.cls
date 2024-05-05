@isTest
private class SwagAmountTest {
    @isTest
    private static void equalsSameInstance() {
        SwagAmount amount1 = SwagAmount.getExample();
        SwagAmount amount2 = amount1;
        SwagAmount amount3 = new SwagAmount();
        SwagAmount amount4 = amount3;

        System.assert(amount1.equals(amount2));
        System.assert(amount2.equals(amount1));
        System.assert(amount1.equals(amount1));
        System.assert(amount3.equals(amount4));
        System.assert(amount4.equals(amount3));
        System.assert(amount3.equals(amount3));
    }

    @isTest
    private static void equalsIdenticalInstance() {
        SwagAmount amount1 = SwagAmount.getExample();
        SwagAmount amount2 = SwagAmount.getExample();
        SwagAmount amount3 = new SwagAmount();
        SwagAmount amount4 = new SwagAmount();

        System.assert(amount1.equals(amount2));
        System.assert(amount2.equals(amount1));
        System.assert(amount3.equals(amount4));
        System.assert(amount4.equals(amount3));
    }

    @isTest
    private static void notEqualsUnlikeInstance() {
        SwagAmount amount1 = SwagAmount.getExample();
        SwagAmount amount2 = new SwagAmount();

        System.assertEquals(false, amount1.equals(amount2));
        System.assertEquals(false, amount2.equals(amount1));
    }

    @isTest
    private static void notEqualsDifferentType() {
        SwagAmount amount1 = SwagAmount.getExample();
        SwagAmount amount2 = new SwagAmount();

        System.assertEquals(false, amount1.equals('foo'));
        System.assertEquals(false, amount2.equals('foo'));
    }

    @isTest
    private static void notEqualsNull() {
        SwagAmount amount1 = SwagAmount.getExample();
        SwagAmount amount2 = new SwagAmount();
        SwagAmount amount3;

        System.assertEquals(false, amount1.equals(amount3));
        System.assertEquals(false, amount2.equals(amount3));
    }

    @isTest
    private static void consistentHashCodeValue() {
        SwagAmount amount1 = SwagAmount.getExample();
        SwagAmount amount2 = new SwagAmount();

        System.assertEquals(amount1.hashCode(), amount1.hashCode());
        System.assertEquals(amount2.hashCode(), amount2.hashCode());
    }

    @isTest
    private static void equalInstancesHaveSameHashCode() {
        SwagAmount amount1 = SwagAmount.getExample();
        SwagAmount amount2 = SwagAmount.getExample();
        SwagAmount amount3 = new SwagAmount();
        SwagAmount amount4 = new SwagAmount();

        System.assert(amount1.equals(amount2));
        System.assert(amount3.equals(amount4));
        System.assertEquals(amount1.hashCode(), amount2.hashCode());
        System.assertEquals(amount3.hashCode(), amount4.hashCode());
    }
}
