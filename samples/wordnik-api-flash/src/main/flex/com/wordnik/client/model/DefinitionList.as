package com.wordnik.client.model {

import com.wordnik.swagger.common.ListWrapper;
import com.wordnik.client.model.ExampleUsage;
import com.wordnik.client.model.Note;
import com.wordnik.client.model.Citation;
import com.wordnik.client.model.TextPron;
import com.wordnik.client.model.Label;
import com.wordnik.client.model.Related;
public class DefinitionList implements ListWrapper {
        // This declaration below of __obj_class is to force flash compiler to include this class
        private var _definition_obj_class: com.wordnik.client.model.Definition = null;
        [XmlElements(name="definition", type="com.wordnik.client.model.Definition")]
        public var definition: Array = new Array();

        public function getList(): Array{
            return definition;
        }

}
}

