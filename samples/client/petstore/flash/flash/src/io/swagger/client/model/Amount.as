package io.swagger.client.model {

import io.swagger.client.model.Currency;

    [XmlRootNode(name="Amount")]
    public class Amount {
        /* some description  */
        [XmlElement(name="value")]
        public var value: Number = 0.0;
                [XmlElement(name="currency")]
        public var currency: Currency = NaN;

    public function toString(): String {
        var str: String = "Amount: ";
        str += " (value: " + value + ")";
        str += " (currency: " + currency + ")";
        return str;
    }

}

}
