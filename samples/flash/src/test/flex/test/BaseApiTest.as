package test
{
import com.adobe.serialization.json.JSON;
import com.adobe.utils.DateUtil;
import com.wordnik.swagger.common.ApiInvoker;
import com.wordnik.swagger.common.ApiUserCredentials;
import com.wordnik.swagger.common.SwaggerApi;
import com.wordnik.swagger.event.ApiClientEvent;
import com.wordnik.swagger.event.Response;
import com.wordnik.client.api.PetApi;
import com.wordnik.client.model.Pet;
import com.wordnik.client.model.PetList;

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
import com.wordnik.client.model.User;

import flexunit.framework.TestCase;

import mx.core.ClassFactory;
import mx.rpc.events.FaultEvent;
import mx.utils.StringUtil;

public class BaseApiTest extends TestCase
{
    protected var cred: ApiUserCredentials = new ApiUserCredentials("localhost:8002", "/api", "special-key");

    protected function applicationExit():void {
        var exitingEvent:Event = new Event(Event.EXITING, false, true);
        NativeApplication.nativeApplication.dispatchEvent(exitingEvent);
        if (!exitingEvent.isDefaultPrevented()) {
            NativeApplication.nativeApplication.exit();
        }
    }

    protected function validateResponse(method: String, e: ApiClientEvent): void {
        trace(method + " " + e.response);
        assertTrue(method + " did not succeed", e.response.isSuccess);
    }

    protected function getTestUser(username: String = null): User {
        var newUser = new User()
        newUser.firstName = "Max";
        newUser.lastName = "Payne";
        newUser.username = username == null ? "user-" + Math.random() : username;
        newUser.phone = "4489989797";
        newUser.email = "maxpayne@hiltonresorts.com";
        newUser.userStatus = "available";
        newUser.password = "XXXXXXXXXXX";

        return newUser;
    }

}
}