package io.swagger.api;

import org.springframework.stereotype.Controller;



@Controller
public class UserApiController implements UserApi {



    public Callable<ResponseEntity<Void>> createUser(@ApiParam(value = "Created user object" ,required=true ) @RequestBody User body) {
        // do some magic!
        return new Callable<ResponseEntity<Void>>() {
            @Override
            public ResponseEntity<Void> call() throws Exception {
                return new ResponseEntity<Void>(HttpStatus.OK);
            }
        };
    }

    public Callable<ResponseEntity<Void>> createUsersWithArrayInput(@ApiParam(value = "List of user object" ,required=true ) @RequestBody List<User> body) {
        // do some magic!
        return new Callable<ResponseEntity<Void>>() {
            @Override
            public ResponseEntity<Void> call() throws Exception {
                return new ResponseEntity<Void>(HttpStatus.OK);
            }
        };
    }

    public Callable<ResponseEntity<Void>> createUsersWithListInput(@ApiParam(value = "List of user object" ,required=true ) @RequestBody List<User> body) {
        // do some magic!
        return new Callable<ResponseEntity<Void>>() {
            @Override
            public ResponseEntity<Void> call() throws Exception {
                return new ResponseEntity<Void>(HttpStatus.OK);
            }
        };
    }

    public Callable<ResponseEntity<Void>> deleteUser(@ApiParam(value = "The name that needs to be deleted",required=true ) @PathVariable("username") String username) {
        // do some magic!
        return new Callable<ResponseEntity<Void>>() {
            @Override
            public ResponseEntity<Void> call() throws Exception {
                return new ResponseEntity<Void>(HttpStatus.OK);
            }
        };
    }

    public Callable<ResponseEntity<User>> getUserByName(@ApiParam(value = "The name that needs to be fetched. Use user1 for testing. ",required=true ) @PathVariable("username") String username) {
        // do some magic!
        return new Callable<ResponseEntity<User>>() {
            @Override
            public ResponseEntity<User> call() throws Exception {
                return new ResponseEntity<User>(HttpStatus.OK);
            }
        };
    }

    public Callable<ResponseEntity<String>> loginUser(@ApiParam(value = "The user name for login", required = true) @RequestParam(value = "username", required = true) String username,
        @ApiParam(value = "The password for login in clear text", required = true) @RequestParam(value = "password", required = true) String password) {
        // do some magic!
        return new Callable<ResponseEntity<String>>() {
            @Override
            public ResponseEntity<String> call() throws Exception {
                return new ResponseEntity<String>(HttpStatus.OK);
            }
        };
    }

    public Callable<ResponseEntity<Void>> logoutUser() {
        // do some magic!
        return new Callable<ResponseEntity<Void>>() {
            @Override
            public ResponseEntity<Void> call() throws Exception {
                return new ResponseEntity<Void>(HttpStatus.OK);
            }
        };
    }

    public Callable<ResponseEntity<Void>> updateUser(@ApiParam(value = "name that need to be deleted",required=true ) @PathVariable("username") String username,
        @ApiParam(value = "Updated user object" ,required=true ) @RequestBody User body) {
        // do some magic!
        return new Callable<ResponseEntity<Void>>() {
            @Override
            public ResponseEntity<Void> call() throws Exception {
                return new ResponseEntity<Void>(HttpStatus.OK);
            }
        };
    }

}
