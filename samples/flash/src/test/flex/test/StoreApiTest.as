package test
{
import com.adobe.serialization.json.JSON;
import com.adobe.utils.DateUtil;
import com.wordnik.swagger.common.ApiInvoker;
import com.wordnik.swagger.common.ApiUserCredentials;
import com.wordnik.swagger.common.SwaggerApi;
import com.wordnik.swagger.event.ApiClientEvent;
import com.wordnik.swagger.event.Response;
import com.wordnik.client.api.UserApi;
import com.wordnik.client.api.StoreApi;
import com.wordnik.client.model.User;
import com.wordnik.client.model.UserList;
import com.wordnik.client.model.Order;
import com.wordnik.client.model.OrderList;

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

// Not testing delete order since it uses Http DELETE
// and thats not supported by flash runtime
// there ain't no blazeds proxy on the server to enable that.
public class StoreApiTest extends BaseApiTest
{
    private var storeApi: StoreApi;

    public function testStoreApi():void {
        trace("StoreApiTest");

        var eventListener: EventDispatcher = new EventDispatcher();
        eventListener.addEventListener(StoreApi.event_getOrderById, on_getOrderById);
//        eventListener.addEventListener(StoreApi.event_deleteOrder, on_deleteOrder);
        eventListener.addEventListener(StoreApi.event_placeOrder, on_placeOrder);

        storeApi = new StoreApi(cred, eventListener);

        trace("Calling getStoreById...");
        storeApi.getOrderById("1");
    }

    public function on_getOrderById(e: ApiClientEvent): void {
        validateResponse("StoreApiTest.getOrderById", e);

        var order: Order = e.response.payload as Order;
        assertTrue("placeOrder did not get the right order", order.id == 1);
        assertTrue("placeOrder did not get the right order", order.status == "placed");

        var newOrder: Order = new Order();
        newOrder.petId = 1;
        newOrder.quantity = 2;

        // next
        storeApi.placeOrder(newOrder)
    }

    public function on_placeOrder(e: ApiClientEvent): void {
        validateResponse("StoreApiTest.placeOrder", e);
    }


}
}