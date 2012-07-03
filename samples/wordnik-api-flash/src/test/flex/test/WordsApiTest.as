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

public class WordsApiTest extends BaseApiTest {
    private var wordsApi:WordsApi;

    public function testWordsApi():void {
        trace("WordsApiTest");

        var eventListener:EventDispatcher = new EventDispatcher();
        eventListener.addEventListener(WordsApi.event_searchWords, on_searchWords);
        eventListener.addEventListener(WordsApi.event_getWordOfTheDay, on_getWordOfTheDay);
        eventListener.addEventListener(WordsApi.event_reverseDictionary, on_reverseDictionary);
        eventListener.addEventListener(WordsApi.event_getRandomWords, on_getRandomWords);
        eventListener.addEventListener(WordsApi.event_getRandomWord, on_getRandomWord);

        wordsApi = new WordsApi(cred, eventListener);
        wordsApi.useProxyServer(super.useProxy);

        wordsApi.searchWords("free", null, null, null, null, null, null, null, null, null, null)
    }

    public function on_searchWords(e:ApiClientEvent):void {
        validateResponse("WordsApiTest.searchWords", e);

        var wordSearchResults: WordSearchResults = e.response.payload as WordSearchResults;
        assertTrue("searchWords did not work", wordSearchResults != null);
        assertTrue("searchWords did not get any results", wordSearchResults.searchResults.length > 0);

        // next
        wordsApi.getWordOfTheDay(null);
    }

    public function on_getWordOfTheDay(e:ApiClientEvent):void {
        validateResponse("WordsApiTest.getWordOfTheDay", e);

        var wordOfTheDay: WordOfTheDay = e.response.payload as WordOfTheDay;
        assertTrue("wordOfTheDay did not work", wordOfTheDay != null);

        // next
        wordsApi.reverseDictionary("free", null, null, null, null, null, null, null, null, null, null, null, null, null);
    }

    public function on_reverseDictionary(e:ApiClientEvent):void {
        validateResponse("WordsApiTest.reverseDictionary", e);

        var definitionSearchResults: DefinitionSearchResults = e.response.payload as DefinitionSearchResults;
        assertTrue("reverseDictionary did not work", definitionSearchResults != null);
        assertTrue("reverseDictionary did not work", definitionSearchResults.results.length > 0);

        // next
        wordsApi.getRandomWords(null, null, null, null, null, null, null, null, null, null, null);
    }

    public function on_getRandomWords(e:ApiClientEvent):void {
        validateResponse("WordsApiTest.getRandomWords", e);

        var randomWords: Array = e.response.payload as Array;
        assertTrue("getRandomWords did not get any results", randomWords.length > 0);

        // next
        wordsApi.getRandomWord(null, null, null, null, null, null, null, null);
    }

    public function on_getRandomWord(e:ApiClientEvent):void {
        validateResponse("WordsApiTest.getRandomWord", e);


    }

}
}