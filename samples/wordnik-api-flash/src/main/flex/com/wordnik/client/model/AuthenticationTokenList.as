package com.wordnik.client.model {

import com.wordnik.swagger.common.ListWrapper;
public class AuthenticationTokenList implements ListWrapper {
        // This declaration below of __obj_class is to force flash compiler to include this class
        private var _authenticationToken_obj_class: com.wordnik.client.model.AuthenticationToken = null;
        [XmlElements(name="authenticationToken", type="com.wordnik.client.model.AuthenticationToken")]
        public var authenticationToken: Array = new Array();

        public function getList(): Array{
            return authenticationToken;
        }

}
}

