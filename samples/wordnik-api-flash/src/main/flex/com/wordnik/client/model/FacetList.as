package com.wordnik.client.model {

import com.wordnik.swagger.common.ListWrapper;
import com.wordnik.client.model.FacetValue;
public class FacetList implements ListWrapper {
        // This declaration below of __obj_class is to force flash compiler to include this class
        private var _facet_obj_class: com.wordnik.client.model.Facet = null;
        [XmlElements(name="facet", type="com.wordnik.client.model.Facet")]
        public var facet: Array = new Array();

        public function getList(): Array{
            return facet;
        }

}
}

