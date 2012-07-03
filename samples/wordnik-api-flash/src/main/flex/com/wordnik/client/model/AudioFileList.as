package com.wordnik.client.model {

import com.wordnik.swagger.common.ListWrapper;
public class AudioFileList implements ListWrapper {
        // This declaration below of __obj_class is to force flash compiler to include this class
        private var _audioFile_obj_class: com.wordnik.client.model.AudioFile = null;
        [XmlElements(name="audioFile", type="com.wordnik.client.model.AudioFile")]
        public var audioFile: Array = new Array();

        public function getList(): Array{
            return audioFile;
        }

}
}

