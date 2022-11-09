package io.swagger.client.model {

import io.swagger.common.ListWrapper;

    public class CurrencyList implements ListWrapper {
        // This declaration below of _Currency_obj_class is to force flash compiler to include this class
        private var _currency_obj_class: io.swagger.client.model.Currency = null;
        [XmlElements(name="currency", type="io.swagger.client.model.Currency")]
        public var currency: Array = new Array();

        public function getList(): Array{
            return currency;
        }

}

}
