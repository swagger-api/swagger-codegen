package io.swagger.event
{

    import flash.events.Event;

    public class ApiClientEvent extends Event
    {
        /*
         * The Response object which contains response info
         */
        public var response:Response;

        public function ApiClientEvent( type:String, bubbles:Boolean = false, cancelable:Boolean = false )
        {
            super( type, bubbles, cancelable );
        }
    }
}