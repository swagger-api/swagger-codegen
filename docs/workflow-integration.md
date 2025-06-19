# Workflow Integrations

## Maven Integration

You can use the [swagger-codegen-maven-plugin](modules/swagger-codegen-maven-plugin/README.md) for integrating with your workflow, and generating any codegen target.

## Gradle Integration

[Gradle Swagger Generator Plugin](https://github.com/int128/gradle-swagger-generator-plugin) is available for generating source code and API document.

## GitHub Integration

To push the auto-generated SDK to GitHub, we provide `git_push.sh` to streamline the process. For example:

 1) Create a new repository in GitHub (Ref: https://help.github.com/articles/creating-a-new-repository/)

 2) Generate the SDK

```sh
 java -jar modules/swagger-codegen-cli/target/swagger-codegen-cli.jar generate \
 -i modules/swagger-codegen/src/test/resources/2_0/petstore.json -l perl \
 --git-user-id "swaggerapi" \
 --git-repo-id "petstore-perl" \
 --release-note "Github integration demo" \
 -o /var/tmp/perl/petstore
```

 3) Push the SDK to GitHub

```sh
cd /var/tmp/perl/petstore
/bin/sh ./git_push.sh
```
