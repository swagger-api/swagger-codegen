package com.wordnik.client.model {

import com.wordnik.swagger.common.ListWrapper;
public class UserList implements ListWrapper {
        [XmlElements(name="user", type="com.wordnik.client.model.User")]
        public var user: Array = new Array();

        public function getList(): Array{
            return user;
        }

}
}

