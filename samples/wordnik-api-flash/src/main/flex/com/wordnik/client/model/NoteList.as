package com.wordnik.client.model {

import com.wordnik.swagger.common.ListWrapper;
public class NoteList implements ListWrapper {
        // This declaration below of __obj_class is to force flash compiler to include this class
        private var _note_obj_class: com.wordnik.client.model.Note = null;
        [XmlElements(name="note", type="com.wordnik.client.model.Note")]
        public var note: Array = new Array();

        public function getList(): Array{
            return note;
        }

}
}

