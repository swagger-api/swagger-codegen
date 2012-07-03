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
    import com.wordnik.client.model.User;
    import com.wordnik.client.model.UserList;

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

    public class UserApiTest extends BaseApiTest
    {
        private var userApi: UserApi;

        public function testUserApi():void{
            trace("UserApiTest");

            var eventListener: EventDispatcher = new EventDispatcher();
            eventListener.addEventListener(UserApi.event_loginUser, on_loginUser);
            eventListener.addEventListener(UserApi.event_logoutUser, on_logoutUser);
            eventListener.addEventListener(UserApi.event_getUserByName, on_getUserByName);
            eventListener.addEventListener(UserApi.event_createUser, on_createUser);
            eventListener.addEventListener(UserApi.event_createUsersWithArrayInput, on_createUsersWithArrayInput);
            eventListener.addEventListener(UserApi.event_createUsersWithListInput, on_createUsersWithListInput);
            eventListener.addEventListener(UserApi.event_updateUser, on_updateUser);
            eventListener.addEventListener(UserApi.event_deleteUser, on_deleteUser);

            userApi = new UserApi(cred, eventListener);

            trace("Calling getUserById...");
            userApi.loginUser("user1", "XXXXXXXXXXX");
        }

        public function on_loginUser(e: ApiClientEvent): void {
            validateResponse("UserApiTest.loginUser", e);

            // next
            userApi.logoutUser();
        }

        public function on_logoutUser(e: ApiClientEvent): void {
            validateResponse("UserApiTest.logoutUser", e);

            // next
            userApi.getUserByName("user1");
        }

        public function on_getUserByName(e: ApiClientEvent): void {
            validateResponse("UserApiTest.getUserByName", e);
            var user: User = e.response.payload as User;
            assertTrue("getUserByName did not get the right user", user.username == "user1");

            // next
            userApi.createUser(getTestUser("maxpayne"));
        }

        public function on_createUser(e: ApiClientEvent): void {
            validateResponse("UserApiTest.createUser", e);

            // next
            var usersToCreate: Array = new Array();
            usersToCreate.push(getTestUser(), getTestUser(), getTestUser(), getTestUser())
            userApi.createUsersWithArrayInput(usersToCreate);
        }

        public function on_createUsersWithArrayInput(e: ApiClientEvent): void {
            validateResponse("UserApiTest.createUsersWithArrayInput", e);

            // next
            var usersToCreate: Array = new Array();
            usersToCreate.push(getTestUser(), getTestUser(), getTestUser(), getTestUser())
            userApi.createUsersWithListInput(usersToCreate);
        }

        public function on_createUsersWithListInput(e: ApiClientEvent): void {
            validateResponse("UserApiTest.createUsersWithListInput", e);

            // next
            userApi.updateUser("maxpayne", getTestUser("maxpayne"));
        }

        public function on_updateUser(e: ApiClientEvent): void {
            validateResponse("UserApiTest.updateUser", e);

            // next
            userApi.deleteUser("maxpayne");
        }

        public function on_deleteUser(e: ApiClientEvent): void {
            validateResponse("UserApiTest.deleteUser", e);

        }


    }
}