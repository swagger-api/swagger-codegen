package = "petstore"
version = "1.0.0-1"
source = {
	url = "https://github.com/GIT_USER_ID/GIT_REPO_ID.git"
}

description = {
	summary = "API client genreated by Swagger Codegen",
	detailed = [[
This is a sample server Petstore server.  You can find out more about Swagger at [http://swagger.io](http://swagger.io) or on [irc.freenode.net, #swagger](http://swagger.io/irc/).  For this sample, you can use the api key `special-key` to test the authorization filters.]],
	homepage = "https://github.com/swagger-api/swagger-codegen",
	license = "Unlicense",
	maintainer = "Swagger Codegen contributors",
}

dependencies = {
	"lua >= 5.2"
}

build = {
	type = "builtin",
	modules = {
		["http.request"] = "http/request.lua";
		["http.util"] = "http/util.lua";
		["dkjson"] = "dkjson.lua";
		["basexx"] = "basexx.lua";
	}
}
