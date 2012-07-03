package test {
import com.adobe.serialization.json.JSON;
import com.adobe.utils.DateUtil;
import com.wordnik.swagger.common.ApiInvoker;
import com.wordnik.swagger.common.ApiUserCredentials;
import com.wordnik.swagger.common.SwaggerApi;
import com.wordnik.swagger.event.ApiClientEvent;
import com.wordnik.swagger.event.Response;
import com.wordnik.client.api.*;

import flash.desktop.NativeApplication;
import flash.events.Event;
import flash.events.EventDispatcher;
import flash.events.IEventDispatcher;
import flash.events.IOErrorEvent;
import flash.filesystem.File;
import flash.filesystem.FileMode;
import flash.filesystem.FileStream;
import flash.net.URLLoader;
import flash.net.URLRequest;
import flash.system.System;
import flash.utils.describeType;
import flash.utils.getDefinitionByName;

import flexunit.framework.TestCase;

import mx.core.ClassFactory;
import mx.rpc.events.FaultEvent;
import mx.utils.StringUtil;

import asaxb.xml.bind.ASAXBContext;
import asaxb.xml.bind.Unmarshaller;
import flash.xml.XMLDocument;
import flash.xml.XMLNode;
import com.wordnik.client.model.*;

public class WordListApiTest extends BaseApiTest {
    private var wordListApi:WordListApi;

    public function testWordListApi():void {
        trace("WordListApiTest");

        var eventListener:EventDispatcher = new EventDispatcher();
//        eventListener.addEventListener(WordListApi.event_getWordListByPermalink, on_getWordListByPermalink);
        eventListener.addEventListener(WordListApi.event_getWordListWords, on_getWordListWords);
//        eventListener.addEventListener(WordListApi.event_addWordsToWordList, on_addWordsToWordList);
//        eventListener.addEventListener(WordListApi.event_deleteWordsFromWordList, on_deleteWordsFromWordList);
//        eventListener.addEventListener(WordListApi.event_updateWordList, on_updateWordList);
//        eventListener.addEventListener(WordListApi.event_deleteWordList, on_deleteWordList);

        wordListApi = new WordListApi(cred, eventListener);
        wordListApi.useProxyServer(super.useProxy);

        trace("wordListApi.getWordListWords")
        wordListApi.getWordListWords("numeronyms", 0, 10, null)
    }

    public function on_getWordListWords(e:ApiClientEvent):void {
        validateResponse("WordsApiTest.getWordListWords", e);

    }


}
}