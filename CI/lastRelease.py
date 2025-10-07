#!/usr/bin/python

import ghApiClient

def getLastReleaseTag():
    content = ghApiClient.readUrl('repos/swagger-api/swagger-codegen/releases')
    for l in content:
        draft = l["draft"]
        tag = l["tag_name"]
        if str(draft) != 'True' and tag.startswith("v2"):
            return tag[1:]

# main
def main():
    result = getLastReleaseTag()
    print (result)

# here start main
main()
