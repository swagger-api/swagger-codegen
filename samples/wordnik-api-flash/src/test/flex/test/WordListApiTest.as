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
    private var wordListsApi:WordListsApi;
    private var accountApi:AccountApi;
    private var authenticationToken: AuthenticationToken;
    private var userWordLists: Array;
    private var userWordList: WordList;
    private var newWordList: WordList;

    public function testWordListApi():void {
        trace("WordListApiTest");

        var eventListener:EventDispatcher = new EventDispatcher();
        eventListener.addEventListener(AccountApi.event_authenticatePost, on_authenticatePost);
        eventListener.addEventListener(AccountApi.event_getWordListsForLoggedInUser, on_getWordListsForLoggedInUser);

        eventListener.addEventListener(WordListsApi.event_createWordList, on_createWordList);

        eventListener.addEventListener(WordListApi.event_getWordListByPermalink, on_getWordListByPermalink);
        eventListener.addEventListener(WordListApi.event_getWordListWords, on_getWordListWords);
        eventListener.addEventListener(WordListApi.event_addWordsToWordList, on_addWordsToWordList);
        eventListener.addEventListener(WordListApi.event_deleteWordsFromWordList, on_deleteWordsFromWordList);
        eventListener.addEventListener(WordListApi.event_updateWordList, on_updateWordList);
        eventListener.addEventListener(WordListApi.event_deleteWordList, on_deleteWordList);

        wordListApi = new WordListApi(cred, eventListener);
        wordListApi.useProxyServer(super.useProxy);
        accountApi = new AccountApi(cred, eventListener);
        accountApi.useProxyServer(super.useProxy);
        wordListsApi = new WordListsApi(cred, eventListener);
        wordListsApi.useProxyServer(super.useProxy);

        trace("wordListApi.getWordListWords")
        accountApi.authenticatePost(username, password)
    }

    public function on_authenticatePost(e:ApiClientEvent):void {
        validateResponse("AccountApiTest.authenticatePost", e);
        this.authenticationToken = e.response.payload as AuthenticationToken;
        assertTrue("AccountApiTest.authenticate did not get authenticationToken", this.authenticationToken != null);

        // next
        accountApi.getWordListsForLoggedInUser(this.authenticationToken.token, 0, 10);

    }

    public function on_getWordListsForLoggedInUser(e:ApiClientEvent):void {
        validateResponse("AccountApiTest.getWordListsForLoggedInUser", e);

        this.userWordLists = e.response.payload as Array;
        assertTrue("AccountApiTest.getWordListsForLoggedInUser did not get any lists", this.userWordLists != null && this.userWordLists.length > 0)
        this.userWordList = userWordLists[0];
        trace("running tests using list " + this.userWordList.permalink);

        // next
        var wordList = new WordList();
        wordList.username = username;
        wordList.description = "flash test list";
        wordList.name = "flash.test.list";
        wordList.type = "PUBLIC";
        wordList.userId = this.authenticationToken.userId;
        this.wordListsApi.createWordList(wordList, this.authenticationToken.token);

    }

    public function on_createWordList(e:ApiClientEvent):void {
        validateResponse("WordsApiTest.createWordList", e);
        this.newWordList = e.response.payload as WordList;
        assertTrue("WordsApiTest.createWordList did not get the created list", this.newWordList != null)

        //next
        wordListApi.getWordListByPermalink(this.newWordList.permalink, this.authenticationToken.token)

    }


    public function on_getWordListByPermalink(e:ApiClientEvent):void {
        validateResponse("WordsApiTest.getWordListByPermalink", e);
        var wordList: WordList = e.response.payload as WordList;
        assertTrue("WordsApiTest.getWordListByPermalink did not get the expected list", wordList != null && wordList.permalink == this.newWordList.permalink)

        //next
        wordListApi.getWordListWords(this.newWordList.permalink, 0, 10, this.authenticationToken.token)

//        todo: add words to list isn't working as it requires username query param and the api listing does not provide it.
//        var wordsToAdd: Array = new Array();
//        wordsToAdd.push(sv("w1"));
//        wordsToAdd.push(sv("w2"));
//        wordListApi.addWordsToWordList(this.newWordList.permalink, wordsToAdd, this.authenticationToken.token)

    }

    public function on_addWordsToWordList(e:ApiClientEvent):void {
        validateResponse("WordsApiTest.addWordsToWordList", e);

        // next
        wordListApi.getWordListWords(this.newWordList.permalink, 0, 10, this.authenticationToken.token)
    }

    public function on_getWordListWords(e:ApiClientEvent):void {
        validateResponse("WordsApiTest.getWordListWords", e);
        var wordListWords: Array = e.response.payload as Array;
//        assertTrue("WordsApiTest.getWordListWords did not get any wordListWords", wordListWords.length > 0);

        // next
        var wordsToDelete: Array = new Array();
        wordsToDelete.push(sv("w1"));
        wordListApi.deleteWordsFromWordList(this.newWordList.permalink, wordsToDelete, this.authenticationToken.token)
    }

    public function on_deleteWordsFromWordList(e:ApiClientEvent):void {
        validateResponse("WordsApiTest.deleteWordsFromWordList", e);

        // next

        wordListApi.deleteWordList(this.newWordList.permalink, this.authenticationToken.token);

//        this.newWordList.createdAt = null;
//        this.newWordList.lastActivityAt = null;
//        this.newWordList.updatedAt = null;
//        this.newWordList.description = "xxx";
//        wordListApi.updateWordList(this.newWordList.permalink, this.newWordList, this.authenticationToken.token)
    }

    public function on_updateWordList(e:ApiClientEvent):void {
        validateResponse("WordsApiTest.updateWordList", e);

        wordListApi.deleteWordList(this.newWordList.permalink, this.authenticationToken.token);
    }

    public function on_deleteWordList(e:ApiClientEvent):void {
        validateResponse("WordsApiTest.deleteWordList", e);

    }

    private function sv(w): StringValue {
        var _sv = new StringValue();
        _sv.word = w;
        return _sv;
    }


}
}