-- https://github.com/swagger-api/swagger-codegen/issues/4794#issuecomment-320245838

local http_request = require "http.request"

local base = "http://petstore.swagger.io:80/v2"

local petId = 4

local req = http_request.new_from_uri(string.format("%s/pet/%d", base, petId))
req.headers:upsert("accept", "application/json")
local headers, stream = req:go()
headers:dump()
print()
print(stream:get_body_as_string())
