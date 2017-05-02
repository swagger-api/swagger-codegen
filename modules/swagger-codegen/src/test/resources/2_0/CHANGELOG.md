Test Resources for Clients
==========================

Test resources have been split and versioned to allow incremental improvements to clients as they evolve towards full compliance to the specification.

Resources
---------

 - **./petstore.yaml**
	 - Petstore specification only.
	 - Good starting point for a new client.
 - **./fake-endpoints-v1/petstore.yaml**
	 - Petstore specification.
	 - All primitive data types.
	 - Primitive body parameters.
	 - Snake case classes.
	 - Classes beginning with numeric or symbols.
	 - PATCH method.
	 - Enum parameters.
	 - Unicode descriptions.
	 - Map Models (except 2D map of enum).
	 - Array Models (except enum).
 - **./fake-endpoints-v2/petstore.yaml**
	 - Recursive models in operations.
	 - map of map of enum.
	 - Array of enum and array of array of enum.
	 - Array parameters in body and query string.
	 - Objects in POST body.

