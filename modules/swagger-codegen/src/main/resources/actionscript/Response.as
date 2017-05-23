package io.swagger.event
{

    /**
     * Response contains info on the result of an API invocation.
     * A completion listener will expect this Response object.
     */
    public class Response
    {

        /**
         * Indicates whether the invoked operation failed or succeeded
         */
        public var isSuccess:Boolean;

        /**
         * The payload of the succesful operation eg. a Word in a WordRequest
         */
        public var payload:Object;

        /**
         * HTTP Response code
         */
        public var httpCode:int;

        public function Response( isSuccessful:Boolean, payload:Object = null, httpCode:int = 0 )
        {
            this.isSuccess = isSuccessful;
            this.payload = payload;
            this.httpCode = httpCode;
        }

        public function toString():String
        {
            return "Response (isSuccess:" + isSuccess + "; httpCode:" + httpCode + "; payload:" + payload + ")";
        }
    }
}