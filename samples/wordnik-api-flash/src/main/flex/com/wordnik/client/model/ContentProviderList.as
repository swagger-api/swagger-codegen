package com.wordnik.client.model {

import com.wordnik.swagger.common.ListWrapper;
public class ContentProviderList implements ListWrapper {
        // This declaration below of __obj_class is to force flash compiler to include this class
        private var _contentProvider_obj_class: com.wordnik.client.model.ContentProvider = null;
        [XmlElements(name="contentProvider", type="com.wordnik.client.model.ContentProvider")]
        public var contentProvider: Array = new Array();

        public function getList(): Array{
            return contentProvider;
        }

}
}

