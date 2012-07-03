package com.wordnik.client.model {

import com.wordnik.swagger.common.ListWrapper;
public class ApiTokenStatusList implements ListWrapper {
        // This declaration below of __obj_class is to force flash compiler to include this class
        private var _apiTokenStatus_obj_class: com.wordnik.client.model.ApiTokenStatus = null;
        [XmlElements(name="apiTokenStatus", type="com.wordnik.client.model.ApiTokenStatus")]
        public var apiTokenStatus: Array = new Array();

        public function getList(): Array{
            return apiTokenStatus;
        }

}
}

