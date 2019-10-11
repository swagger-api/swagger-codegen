package io.swagger.client.model {

import io.swagger.common.ListWrapper;
import io.swagger.client.model.Currency;

    public class AmountList implements ListWrapper {
        // This declaration below of _Amount_obj_class is to force flash compiler to include this class
        private var _amount_obj_class: io.swagger.client.model.Amount = null;
        [XmlElements(name="amount", type="io.swagger.client.model.Amount")]
        public var amount: Array = new Array();

        public function getList(): Array{
            return amount;
        }

}

}
