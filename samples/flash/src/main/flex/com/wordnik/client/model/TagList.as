package com.wordnik.client.model {

import com.wordnik.swagger.common.ListWrapper;
public class TagList implements ListWrapper {
        [XmlElements(name="tag", type="com.wordnik.client.model.Tag")]
        public var tag: Array = new Array();

        public function getList(): Array{
            return tag;
        }

}
}

