package com.wordnik.client.model {

import com.wordnik.swagger.common.ListWrapper;
import com.wordnik.client.model.Sentence;
import com.wordnik.client.model.ScoredWord;
import com.wordnik.client.model.ContentProvider;
public class ExampleList implements ListWrapper {
        // This declaration below of __obj_class is to force flash compiler to include this class
        private var _example_obj_class: com.wordnik.client.model.Example = null;
        [XmlElements(name="example", type="com.wordnik.client.model.Example")]
        public var example: Array = new Array();

        public function getList(): Array{
            return example;
        }

}
}

