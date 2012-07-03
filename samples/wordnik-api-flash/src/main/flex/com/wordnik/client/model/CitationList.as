package com.wordnik.client.model {

import com.wordnik.swagger.common.ListWrapper;
public class CitationList implements ListWrapper {
        // This declaration below of __obj_class is to force flash compiler to include this class
        private var _citation_obj_class: com.wordnik.client.model.Citation = null;
        [XmlElements(name="citation", type="com.wordnik.client.model.Citation")]
        public var citation: Array = new Array();

        public function getList(): Array{
            return citation;
        }

}
}

