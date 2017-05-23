/**
 * Created by tbilou on 06/06/16.
 */
package io.swagger
{

	import flash.events.Event;
	import flash.events.EventDispatcher;
	import flash.events.HTTPStatusEvent;
	import flash.events.IOErrorEvent;
	import flash.events.SecurityErrorEvent;
	import flash.net.URLLoader;
	import flash.net.URLLoaderDataFormat;
	import flash.net.URLRequest;
	import flash.net.URLRequestHeader;
	import flash.net.URLRequestMethod;
	import flash.net.URLVariables;
	import flash.utils.Dictionary;

	import io.swagger.event.Response;

	public class HttpClient
	{
		public var hostname:String;

		private var _dispatcher:EventDispatcher;
		private var _callback:Function;

		private var dataFormat:String = URLLoaderDataFormat.TEXT;
		private var loader:URLLoader;

		public function HttpClient( hostname:String, eventDispatcher:EventDispatcher )
		{
			this.hostname = hostname;
			this._dispatcher = eventDispatcher;

			loader = new URLLoader();
			loader.dataFormat = dataFormat;
			configureListeners( loader );
		}

		public function call( path:String, callback:Function, queryParams:Dictionary, headerParams:Dictionary ):void
		{
			var requestUrl:String = hostname + path;

			var request:URLRequest = new URLRequest( requestUrl );
			request.method = URLRequestMethod.GET;

			try
			{
				_callback = callback;

				// Configure queryParams
				var variables:URLVariables = new URLVariables();
				for (var paramName:String in queryParams) {
					variables[paramName] = queryParams[paramName];
				}
				request.data = variables;

				// Configure headers
				var headers:Array = [];
				for (var headerParam:String in headerParams) {
					headers.push(new URLRequestHeader(headerParam, headerParams[headerParam]));
				}
				request.requestHeaders = headers;


				// Make the request
				loader.load( request );
			}
			catch (error:Error)
			{
				trace( "Error loading requested document: " + requestUrl );
			}
		}

		private function completeHandler( event:Event ):void
		{
			var loader:URLLoader = URLLoader( event.target );
			// Create a response
			var response:Response = new Response( true, loader.data );
			destroyListeners( loader );
			_callback( response );
		}

		private function httpStatusHandler( event:HTTPStatusEvent ):void
		{
			trace( "http status code : " + event.status );
			if (event.status != 200)
			{
				loader.removeEventListener( Event.COMPLETE, completeHandler );
				var response:Response = new Response( false, null, event.status );
				_callback( response );
			}
		}

		private function ioErrorHandler( event:IOErrorEvent ):void
		{
			trace( "ioErrorHandler: " + event );
			destroyListeners( loader );
		}

		private function securityErrorHandler( event:SecurityErrorEvent ):void
		{
			trace( "securityErrorHandler: " + event );
			destroyListeners( loader );
		}

		private function configureListeners( dispatcher:URLLoader ):void
		{
			dispatcher.addEventListener( Event.COMPLETE, completeHandler );
			dispatcher.addEventListener( SecurityErrorEvent.SECURITY_ERROR, securityErrorHandler );
			dispatcher.addEventListener( HTTPStatusEvent.HTTP_STATUS, httpStatusHandler );
			dispatcher.addEventListener( IOErrorEvent.IO_ERROR, ioErrorHandler );
		}

		private function destroyListeners( dispatcher:URLLoader ):void
		{
			dispatcher.removeEventListener( Event.COMPLETE, completeHandler );
			dispatcher.removeEventListener( SecurityErrorEvent.SECURITY_ERROR, securityErrorHandler );
			dispatcher.removeEventListener( HTTPStatusEvent.HTTP_STATUS, httpStatusHandler );
			dispatcher.removeEventListener( IOErrorEvent.IO_ERROR, ioErrorHandler );
		}
	}
}
