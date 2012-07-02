package com.wordnik.client.model {

import com.wordnik.swagger.common.ListWrapper;
public class CategoryList implements ListWrapper {
        [XmlElements(name="category", type="com.wordnik.client.model.Category")]
        public var category: Array = new Array();

        public function getList(): Array{
            return category;
        }

}
}

