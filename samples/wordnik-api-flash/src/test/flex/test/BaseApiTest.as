package test
{
import com.adobe.serialization.json.JSON;
import com.adobe.utils.DateUtil;
import com.wordnik.swagger.common.ApiInvoker;
import com.wordnik.swagger.common.ApiUserCredentials;
import com.wordnik.swagger.common.SwaggerApi;
import com.wordnik.swagger.event.ApiClientEvent;
import com.wordnik.swagger.event.Response;

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

public class BaseApiTest extends TestCase
{
    protected var username = "[username]";
    protected var password = "[password]";
    protected var apiKey = "[api-key]";

//    ELB without proxy
    protected var cred: ApiUserCredentials = new ApiUserCredentials("api.wordnik.com", "/v4", apiKey, null, -1);
    protected var useProxy = false;

//    ELB with proxy
//    protected var cred: ApiUserCredentials = new ApiUserCredentials("api.wordnik.com", "/v4", apiKey, null, -1, "http://apihost.wordnik.com", "/v4/messagebroker/restproxy");
//    protected var useProxy = true;

//    Direct to api-server-2
//    protected var cred: ApiUserCredentials = new ApiUserCredentials("ec2-204-236-137-186.us-west-1.compute.amazonaws.com:8000", "/api", apiKey, null, -1, "http://apihost.wordnik.com", "/messagebroker/restproxy");
//    protected var useProxy = true;

//    Direct to api-server-1
//    protected var cred: ApiUserCredentials = new ApiUserCredentials("ec2-50-18-19-128.us-west-1.compute.amazonaws.com:8000", "/api", apiKey, null, -1, "http://apihost.wordnik.com", "/messagebroker/restproxy");
//    protected var useProxy = true;

//    Beta
//    protected var cred: ApiUserCredentials = new ApiUserCredentials("beta.api.wordnik.com", "/api", apiKey, null, -1, "http://apihost.wordnik.com", "/messagebroker/restproxy");
//    protected var useProxy = true;

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


}
}