#!/usr/bin/python

import sys
import json
from datetime import datetime
import ghApiClient

def allPulls(releaseDate):

    result = ""

    baseurl = "https://api.github.com/repos/swagger-api/swagger-codegen/pulls/"
    content = ghApiClient.readUrl('repos/swagger-api/swagger-codegen/pulls?state=closed&base=master&per_page=100')
    for l in content:
        stripped = l["url"][len(baseurl):]
        mergedAt = l["merged_at"]
        if mergedAt is not None:
            if datetime.strptime(mergedAt, '%Y-%m-%dT%H:%M:%SZ') > releaseDate:
                if not l['title'].startswith("bump snap"):
                    result += '\n'
                    result += "* " + l['title'] + " (#" + stripped + ")"
    return result


def lastReleaseDate(tag):
    content = ghApiClient.readUrl('repos/swagger-api/swagger-codegen/releases/tags/' + tag)
    publishedAt = content["published_at"]
    return datetime.strptime(publishedAt, '%Y-%m-%dT%H:%M:%SZ')


def addRelease(release_title, tag, content):
    payload = "{\"tag_name\":\"" + tag + "\", "
    payload += "\"name\":" + json.dumps(release_title) + ", "
    payload += "\"body\":" + json.dumps(content) + ", "
    payload += "\"draft\":" + "true" + ", "
    payload += "\"prerelease\":" + "false" + ", "
    payload += "\"target_commitish\":\"" + "master" + "\"}"
    content = ghApiClient.postUrl('repos/swagger-api/swagger-codegen/releases', payload)
    return content

def getReleases():
    content = ghApiClient.readUrl('repos/swagger-api/swagger-codegen/releases')
    return content

# main
def main(last_release, release_title, tag):
    result = allPulls(lastReleaseDate('v' + last_release))
    addRelease (release_title, tag, result)

# here start main
main(sys.argv[1], sys.argv[2], sys.argv[3])

