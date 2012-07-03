package com.wordnik.client.model {

import com.wordnik.swagger.common.ListWrapper;
public class RelatedList implements ListWrapper {
        // This declaration below of __obj_class is to force flash compiler to include this class
        private var _related_obj_class: com.wordnik.client.model.Related = null;
        [XmlElements(name="related", type="com.wordnik.client.model.Related")]
        public var related: Array = new Array();

        public function getList(): Array{
            return related;
        }

}
}

