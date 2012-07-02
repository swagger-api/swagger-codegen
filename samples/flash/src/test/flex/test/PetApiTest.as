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

    import flexunit.framework.TestCase;

    import mx.core.ClassFactory;
    import mx.rpc.events.FaultEvent;
    import mx.utils.StringUtil;

    public class PetApiTest extends BaseApiTest
    {
        private var petApi: PetApi;

        public function testPetApi():void{
            trace("PetApiTest");

            var eventListener: EventDispatcher = new EventDispatcher();
            eventListener.addEventListener(PetApi.event_getPetById, on_getPetById);
            eventListener.addEventListener(PetApi.event_addPet, on_addPet);
            eventListener.addEventListener(PetApi.event_updatePet, on_updatePet);
            eventListener.addEventListener(PetApi.event_findPetsByStatus, on_findPetsByStatus);
            eventListener.addEventListener(PetApi.event_findPetsByTags, on_findPetsByTags);

            petApi = new PetApi(cred, eventListener);

            trace("Calling getPetById...");
            petApi.getPetById("1");
        }

        public function on_getPetById(e: ApiClientEvent): void {
            trace("PetApiTest.on_getPetById " + e.response);
            assertTrue("getPetById did not succeed", e.response.isSuccess);
            var pet: Pet = e.response.payload as Pet;
            trace("PetApiTest.on_getPetById for pet " + pet);
            assertTrue("getPetById did not get the expected response", pet.id == 1);

            // next
            trace("Calling updatePet...");
            pet.name = "pogo";
            petApi.updatePet(pet);
        }

        public function on_updatePet(e: ApiClientEvent): void {
            trace("PetApiTest.on_updatePet " + e.response);
            assertTrue("updatePet did not succeed", e.response.isSuccess);

            // next
            trace("Calling addPet...")
            var jojo: Pet = new Pet();
            jojo.name = "Jojo";
            petApi.addPet(jojo);
        }

        public function on_addPet(e: ApiClientEvent): void {
            trace("PetApiTest.on_addPet " + e.response);
            assertTrue("addPet did not succeed", e.response.isSuccess);

            // next
            trace("Calling findPetsByStatus...")
            petApi.findPetsByStatus("available");
        }

        public function on_findPetsByStatus(e: ApiClientEvent): void {
            trace("PetApiTest.on_findPetsByStatus " + e.response);
            assertTrue("findPetsByStatus did not succeed", e.response.isSuccess);
            var pets: Array = e.response.payload as Array;
            trace("PetApiTest.on_findPetsByStatus : " + pets.length);
            assertTrue("findPetsByStatus did not find any pets", pets.length > 0);

            // next
            trace("Calling findPetsByTags...")
            petApi.findPetsByTags("tag1");
        }

        public function on_findPetsByTags(e: ApiClientEvent): void {
            trace("PetApiTest.on_findPetsByTags " + e.response);
            assertTrue("findPetsByTags did not succeed", e.response.isSuccess);
            var pets: Array = e.response.payload as Array;
            trace("PetApiTest.findPetsByTags : " + pets.length);
            assertTrue("findPetsByTags did not find any pets", pets.length > 0);

//            applicationExit();
        }

    }
}