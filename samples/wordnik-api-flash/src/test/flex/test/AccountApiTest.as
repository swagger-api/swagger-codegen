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

public class AccountApiTest extends BaseApiTest {
    private var accountApi:AccountApi;
    private var authenticationToken: AuthenticationToken;

    public function testAccountApi():void {
        trace("AccountApiTest");

        var eventListener:EventDispatcher = new EventDispatcher();
        eventListener.addEventListener(AccountApi.event_authenticate, on_authenticate);
        eventListener.addEventListener(AccountApi.event_authenticatePost, on_authenticatePost);
        eventListener.addEventListener(AccountApi.event_getWordListsForLoggedInUser, on_getWordListsForLoggedInUser);
        eventListener.addEventListener(AccountApi.event_getApiTokenStatus, on_getApiTokenStatus);
        eventListener.addEventListener(AccountApi.event_getLoggedInUser, on_getLoggedInUser);

        accountApi = new AccountApi(cred, eventListener);
        accountApi.useProxyServer(super.useProxy);

        accountApi.authenticate(username, password)
    }

    public function on_authenticate(e:ApiClientEvent):void {
        validateResponse("AccountApiTest.authenticate", e);
        this.authenticationToken = e.response.payload as AuthenticationToken;
        assertTrue("AccountApiTest.authenticate did not get authenticationToken", this.authenticationToken != null);

        // next
        accountApi.authenticatePost(username, password)
    }

    public function on_authenticatePost(e:ApiClientEvent):void {
        validateResponse("AccountApiTest.authenticatePost", e);
        this.authenticationToken = e.response.payload as AuthenticationToken;
        assertTrue("AccountApiTest.authenticate did not get authenticationToken", this.authenticationToken != null);

        // next
        accountApi.getApiTokenStatus(apiKey)

    }

    public function on_getApiTokenStatus(e:ApiClientEvent):void {
        validateResponse("AccountApiTest.getApiTokenStatus", e);
        var apiTokenStatus: ApiTokenStatus = e.response.payload as ApiTokenStatus;
        assertTrue("AccountApiTest.getApiTokenStatus did not return ApiTokenStatus", apiTokenStatus != null)

        // next
        accountApi.getWordListsForLoggedInUser(this.authenticationToken.token, 0, 10);
    }

    public function on_getWordListsForLoggedInUser(e:ApiClientEvent):void {
        validateResponse("AccountApiTest.getWordListsForLoggedInUser", e);

        var list: Array = e.response.payload as Array;
        assertTrue("AccountApiTest.getWordListsForLoggedInUser did not get any lists", list != null && list.length > 0)

        // next
        accountApi.getLoggedInUser(this.authenticationToken.token);

    }

    public function on_getLoggedInUser(e:ApiClientEvent):void {
        validateResponse("AccountApiTest.getLoggedInUser", e);
        var user: User = e.response.payload as User;
        assertTrue("AccountApiTest.getLoggedInUser did not return the expected user", user != null && user.username == username)

    }


}
}