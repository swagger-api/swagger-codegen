package test {
import com.adobe.serialization.json.JSON;
import com.adobe.utils.DateUtil;
import com.wordnik.swagger.common.ApiInvoker;
import com.wordnik.swagger.common.ApiUserCredentials;
import com.wordnik.swagger.common.SwaggerApi;
import com.wordnik.swagger.event.ApiClientEvent;
import com.wordnik.swagger.event.Response;
import com.wordnik.client.api.WordApi;

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

public class WordApiTest extends BaseApiTest {
    private var wordApi:WordApi;

    public function testWordApi():void {
        trace("WordApiTest");

        var eventListener:EventDispatcher = new EventDispatcher();
        eventListener.addEventListener(WordApi.event_getExamples, on_getExamples);
        eventListener.addEventListener(WordApi.event_getWord, on_getWord);
        eventListener.addEventListener(WordApi.event_getDefinitions, on_getDefinitions);
        eventListener.addEventListener(WordApi.event_getTopExample, on_getTopExample);
        eventListener.addEventListener(WordApi.event_getRelatedWords, on_getRelatedWords);
        eventListener.addEventListener(WordApi.event_getTextPronunciations, on_getTextPronunciations);
        eventListener.addEventListener(WordApi.event_getHyphenation, on_getHyphenation);
        eventListener.addEventListener(WordApi.event_getWordFrequency, on_getWordFrequency);
        eventListener.addEventListener(WordApi.event_getPhrases, on_getPhrases);
        eventListener.addEventListener(WordApi.event_getAudio, on_getAudio);

        wordApi = new WordApi(cred, eventListener);
        wordApi.useProxyServer(super.useProxy);

        wordApi.getExamples("free", 0, 2)
    }

    public function on_getExamples(e:ApiClientEvent):void {
        validateResponse("WordApiTest.getExamples", e);

        var exampleSearchResults: ExampleSearchResults = e.response.payload as ExampleSearchResults;
        assertTrue("getExamples did not find any examples", exampleSearchResults.examples.length == 2);

        // next
        wordApi.getWord("free")
    }

    public function on_getWord(e:ApiClientEvent):void {
        validateResponse("WordApiTest.on_getWord", e);
        var wordObject: WordObject = e.response.payload as WordObject;
        assertTrue("getWord did not get the expected word", wordObject.word == "free");

        // next
        wordApi.getDefinitions("free", 10, null, null);
    }

    public function on_getDefinitions(e:ApiClientEvent):void {
        validateResponse("WordApiTest.getDefinitions", e);

        var definitions: Array = e.response.payload as Array;
        assertTrue("getDefinitions did not get definitions", definitions.length > 0);

        // next
        wordApi.getTopExample("free")
    }

    public function on_getTopExample(e:ApiClientEvent):void {
        validateResponse("WordApiTest.getTopExample", e);

        var example: Example = e.response.payload as Example;
        assertTrue("getTopExample did not get an example", example != null);
        assertTrue("getTopExample did not get an example", example.word == "free");

        // next
        wordApi.getRelatedWords("free", null, null)
    }

    public function on_getRelatedWords(e:ApiClientEvent):void {
        validateResponse("WordApiTest.getRelatedWords", e);

        var relatedWords: Array = e.response.payload as Array;
        assertTrue("Did not get relatedWords", relatedWords.length > 0);

        // next
        wordApi.getTextPronunciations("free", null, null, 2);
    }

    public function on_getTextPronunciations(e:ApiClientEvent):void {
        validateResponse("WordApiTest.getTextPronunciations", e);

        var textprons: Array = e.response.payload as Array;
        assertTrue("Did not get textprons", textprons.length > 0);

        // next
        wordApi.getHyphenation("free", null, 10)
    }

    public function on_getHyphenation(e:ApiClientEvent):void {
        validateResponse("WordApiTest.getHyphenation", e);

        var syllables: Array = e.response.payload as Array;
        assertTrue("Did not get syllables", syllables.length > 0);

        // next
//        wordApi.getWordFrequency("free", null, null)
        wordApi.getPhrases("free", 10, null)
    }

    public function on_getWordFrequency(e:ApiClientEvent):void {
        validateResponse("WordApiTest.getWordFrequency", e);

        var frequencySummary: FrequencySummary = e.response.payload as FrequencySummary;
        assertTrue("Did not get frequencySummary", frequencySummary != null);

        // next
        wordApi.getPhrases("free", 10, null)
    }

    public function on_getPhrases(e:ApiClientEvent):void {
        validateResponse("WordApiTest.getPhrases", e);

        var bigrams: Array = e.response.payload as Array;
//        assertTrue("Did not get bigrams", bigrams.length > 0);

        // next
        wordApi.getAudio("free", 5)
    }

    public function on_getAudio(e:ApiClientEvent):void {
        validateResponse("WordApiTest.getAudio", e);

        var audioFiles: Array = e.response.payload as Array;
//        assertTrue("Did not get audioFiles", audioFiles.length > 0);

    }

}
}