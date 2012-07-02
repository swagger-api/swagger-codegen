package com.wordnik.client.model {

import com.wordnik.swagger.common.ListWrapper;
public class OrderList implements ListWrapper {
        [XmlElements(name="order", type="com.wordnik.client.model.Order")]
        public var order: Array = new Array();

        public function getList(): Array{
            return order;
        }

}
}

