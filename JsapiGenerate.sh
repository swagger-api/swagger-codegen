#!/bin/bash
if [ $# -lt 2 ]; then
  echo "USAGE:  JsapiGenerate.sh swagger_doc_url version_number"
  exit 1
fi

JSON_FILE="$1"
VERSION_NUMBER="$2"
BASE_JAR="modules/swagger-codegen-cli/target/swagger-codegen-cli.jar"
GIT_USERNAME="stoutfiles"
GIT_EMAIL="shawn.stout@knetik.com"
DEV_NAME="Shawn Stout"
DEV_EMAIL="admin@knetikcloud.com"
DEV_ORG="Knetik"
DEV_ORG_URL="knetikcloud.com"
BR_2="\n  "
BR_4="\n    "
BR_6="\n      "
POM_ORIGINAL="</scm>"
#POM_REPLACEMENT="</scm>$BR_2<licenses>$BR_4<license>$BR_6<name>The Apache Software License, Version 2.0</name>$BR_6<url>http://www.apache.org/licenses/LICENSE-2.0.txt</url>$BR_4</license>$BR_2</licenses>$BR_2"
POM_REPLACEMENT="</scm>$BR_2<developers>$BR_4<developer>$BR_6<name>$DEV_NAME</name>$BR_6<email>$DEV_EMAIL</email>$BR_6<organization>$DEV_ORG</organization>$BR_6<organizationUrl>$DEV_ORG_URL</organizationUrl>$BR_4</developer>$BR_2</developers>"
README_ORIGINAL="## Getting Started"
README_ORIGINAL_PERL="# GETTING STARTED"
README_REPLACEMENT="## Getting Started \n\n KnetikCloud (JSAPI) uses a strict Oauth 2.0 implementation with the following grant types: \n\n"
README_REPLACEMENT+="* **Password grant**: Used for user authentication, usually from an unsecured web or mobile client when a fully authenticated user account is required to perform actions. ex: \n\n"
README_REPLACEMENT+="\`\`\`curl \nPOST /oauth/token?grant_type=password\&client_id=web\&username=jdoe\&password=68a4sd3sd\n \`\`\` \n\n"
README_REPLACEMENT+="* **Client credentials grant**: \n Used for server authentication or secured clients when the secret key cannot be discovered. This kind of grant is typically used for administrative tasks on the application itself or to access other user's account information. \n\n"
README_REPLACEMENT+="\`\`\`curl \nPOST /oauth/token grant_type=client_credentials\&client_id=server-client-id\&client_secret=1s31dfas65d4f3sa651c3s54f \n\`\`\`  \n\n"
README_REPLACEMENT+="The endpoint will return a response containing the authentication token as follows: \n"
README_REPLACEMENT+="\`\`\`json: \n"
README_REPLACEMENT+="{\"access_token\":\"25a0659c-6f4a-40bd-950e-0ba4af7acf0f\",\"token_type\":\"bearer\",\"expires_in\":2145660769,\"scope\":\"write read\"}\n"
README_REPLACEMENT+="\`\`\` \n\n"
README_REPLACEMENT+="Use the provided access_token in sub-sequent requests to authenticate (see code below). Make sure you refresh your token before it expires to avoid having to re-authenticate."

ID_FLAGS="--group-id com.knetikcloud --artifact-version $VERSION_NUMBER"

mkdir -p sdk
chmod 777 sdk

#Android
mkdir -p sdk/android
chmod 777 sdk/android
cd sdk/android

git init
git config user.name "$GIT_USERNAME"
git config user.email "$GIT_EMAIL"
git remote add origin git@github.com:knetikmedia/knetikcloud-android-client.git
git pull origin master
rm -r *

cd ../..
java -jar $BASE_JAR generate -i $JSON_FILE -DinvokerPackage="com.knetikcloud.client",modelPackage="com.knetikcloud.model",apiPackage="com.knetikcloud.api" -l android $ID_FLAGS --artifact-id knetikcloud-android-client -o sdk/android
cd sdk/android

sed -i -e 's~'"$README_ORIGINAL"'~'"$README_REPLACEMENT"'~g' README.md

git add -A
git commit -m "JSAPI Android API update"
git push -u origin master
cd ../..

#ASPNet5
mkdir -p sdk/aspnet5
chmod 777 sdk/aspnet5
cd sdk/aspnet5

git init
git config user.name "$GIT_USERNAME"
git config user.email "$GIT_EMAIL"
git remote add origin git@github.com:knetikmedia/knetikcloud-aspnet5-client.git
git pull origin master
rm -r *

cd ../..
java -jar $BASE_JAR generate -i $JSON_FILE -l aspnet5 $ID_FLAGS --artifact-id knetikcloud-aspnet5-client -o sdk/aspnet5
cd sdk/aspnet5

git add -A
git commit -m "JSAPI ASPNet5 API update"
git push -u origin master
cd ../..



#Bash
mkdir -p sdk/bash
chmod 777 sdk/bash
cd sdk/bash

git init
git config user.name "$GIT_USERNAME"
git config user.email "$GIT_EMAIL"
git remote add origin git@github.com:knetikmedia/knetikcloud-bash-client.git
git pull origin master
rm -r *

cd ../..
java -jar $BASE_JAR generate -i $JSON_FILE -l bash $ID_FLAGS --artifact-id knetikcloud-bash-client -o sdk/bash
cd sdk/bash

git add -A
git commit -m "JSAPI Bash API update"
git push -u origin master
cd ../..



#Clojure
mkdir -p sdk/clojure
chmod 777 sdk/clojure
cd sdk/clojure

git init
git config user.name "$GIT_USERNAME"
git config user.email "$GIT_EMAIL"
git remote add origin git@github.com:knetikmedia/knetikcloud-clojure-client.git
git pull origin master
rm -r *

cd ../..
java -jar $BASE_JAR generate -i $JSON_FILE -l clojure $ID_FLAGS --artifact-id knetikcloud-clojure-client -o sdk/clojure
cd sdk/clojure

git add -A
git commit -m "JSAPI Clojure API update"
git push -u origin master
cd ../..


#C++ Rest
mkdir -p sdk/cpprest
chmod 777 sdk/cpprest
cd sdk/cpprest

git init
git config user.name "$GIT_USERNAME"
git config user.email "$GIT_EMAIL"
git remote add origin git@github.com:knetikmedia/knetikcloud-cpprest-client.git
git pull origin master
rm -r *

cd ../..
java -jar $BASE_JAR generate -i $JSON_FILE -l cpprest -DmodelPackage="com.knetikcloud.client.model",apiPackage="com.knetikcloud.client.api" $ID_FLAGS --artifact-id knetikcloud-cpprest-client -o sdk/cpprest
cd sdk/cpprest

git add -A
git commit -m "JSAPI Cpprest API update"
git push -u origin master
cd ../..

#CSharp 2.0
mkdir -p sdk/csharp-net20
chmod 777 sdk/csharp-net20
cd sdk/csharp-net20

git init
git config user.name "$GIT_USERNAME"
git config user.email "$GIT_EMAIL"
git remote add origin git@github.com:knetikmedia/knetikcloud-csharp-net20-client.git
git pull origin master
rm -r *

cd ../..
java -jar $BASE_JAR generate -i $JSON_FILE -l CsharpDotNet2 -DpackageName="com.knetikcloud" $ID_FLAGS --artifact-id knetikcloud-csharp-net20-client -o sdk/csharp-net20
cd sdk/csharp-net20

sed -i -e 's~'"$README_ORIGINAL"'~'"$README_REPLACEMENT"'~g' README.md

git add -A
git commit -m "JSAPI C# 2.0 API update"
git push -u origin master
cd ../..


#CSharp 3.5
mkdir -p sdk/csharp-net35
chmod 777 sdk/csharp-net35
cd sdk/csharp-net35

git init
git config user.name "$GIT_USERNAME"
git config user.email "$GIT_EMAIL"
git remote add origin git@github.com:knetikmedia/knetikcloud-csharp-net35-client.git
git pull origin master
rm -r *

cd ../..
java -jar $BASE_JAR generate -i $JSON_FILE -l csharp -DpackageName="com.knetikcloud",targetFramework="v3.5" $ID_FLAGS --artifact-id knetikcloud-csharp-net35-client -o sdk/csharp-net35
cd sdk/csharp-net35

sed -i -e 's~'"$README_ORIGINAL"'~'"$README_REPLACEMENT"'~g' README.md

git add -A
git commit -m "JSAPI C# 3.5 API update"
git push -u origin master
cd ../..


#CSharp 4.0+
mkdir -p sdk/csharp
chmod 777 sdk/csharp
cd sdk/csharp

git init
git config user.name "$GIT_USERNAME"
git config user.email "$GIT_EMAIL"
git remote add origin git@github.com:knetikmedia/knetikcloud-csharp-client.git
git pull origin master
rm -r *

cd ../..
java -jar $BASE_JAR generate -i $JSON_FILE -l csharp -DpackageName="com.knetikcloud" $ID_FLAGS --artifact-id knetikcloud-csharp-client -o sdk/csharp
cd sdk/csharp

sed -i -e 's~'"$README_ORIGINAL"'~'"$README_REPLACEMENT"'~g' README.md

git add -A
git commit -m "JSAPI C# 4.0+ API update"
git push -u origin master
cd ../..


#Dart
mkdir -p sdk/dart
chmod 777 sdk/dart
cd sdk/dart

git init
git config user.name "$GIT_USERNAME"
git config user.email "$GIT_EMAIL"
git remote add origin git@github.com:knetikmedia/knetikcloud-dart-client.git
git pull origin master
rm -r *

cd ../..
java -jar $BASE_JAR generate -i $JSON_FILE -l dart $ID_FLAGS --artifact-id knetikcloud-dart-client -o sdk/dart
cd sdk/dart

sed -i -e 's~'"$README_ORIGINAL"'~'"$README_REPLACEMENT"'~g' README.md

git add -A
git commit -m "JSAPI Dart API update"
git push -u origin master
cd ../..


#Elixir
mkdir -p sdk/elixir
chmod 777 sdk/elixir
cd sdk/elixir

git init
git config user.name "$GIT_USERNAME"
git config user.email "$GIT_EMAIL"
git remote add origin git@github.com:knetikmedia/knetikcloud-elixir-client.git
git pull origin master
rm -r *

cd ../..
java -jar $BASE_JAR generate -i $JSON_FILE -l elixir $ID_FLAGS --artifact-id knetikcloud-elixir-client -o sdk/elixir
cd sdk/elixir

git add -A
git commit -m "JSAPI Elixir API update"
git push -u origin master
cd ../..

#Flash
mkdir -p sdk/flash
chmod 777 sdk/flash
cd sdk/flash

git init
git config user.name "$GIT_USERNAME"
git config user.email "$GIT_EMAIL"
git remote add origin git@github.com:knetikmedia/knetikcloud-flash-client.git
git pull origin master
rm -r *

cd ../..
java -jar $BASE_JAR generate -i $JSON_FILE -l flash $ID_FLAGS --artifact-id knetikcloud-flash-client -o sdk/flash
cd sdk/flash

git add -A
git commit -m "JSAPI Flash API update"
git push -u origin master
cd ../..

#Go
mkdir -p sdk/go
chmod 777 sdk/go
cd sdk/go

git init
git config user.name "$GIT_USERNAME"
git config user.email "$GIT_EMAIL"
git remote add origin git@github.com:knetikmedia/knetikcloud-go-client.git
git pull origin master
rm -r *

cd ../..
java -jar $BASE_JAR generate -i $JSON_FILE -l go $ID_FLAGS --artifact-id knetikcloud-go-client -o sdk/go
cd sdk/go

git add -A
git commit -m "JSAPI Go API update"
git push -u origin master
cd ../..

#Groovy
mkdir -p sdk/groovy
chmod 777 sdk/groovy
cd sdk/groovy

git init
git config user.name "$GIT_USERNAME"
git config user.email "$GIT_EMAIL"
git remote add origin git@github.com:knetikmedia/knetikcloud-groovy-client.git
git pull origin master
rm -r *

cd ../..
java -jar $BASE_JAR generate -i $JSON_FILE -l groovy $ID_FLAGS --artifact-id knetikcloud-groovy-client -o sdk/groovy
cd sdk/groovy

#sed -i -e 's~'"$POM_ORIGINAL"'~'"$POM_REPLACEMENT"'~g' pom.xml

git add -A
git commit -m "JSAPI Groovy API update"
git push -u origin master
cd ../..

#Java
mkdir -p sdk/java
chmod 777 sdk/java
cd sdk/java

git init
git config user.name "$GIT_USERNAME"
git config user.email "$GIT_EMAIL"
git remote add origin git@github.com:knetikmedia/knetikcloud-java-client.git
git pull origin master
rm -r *

cd ../..
java -jar $BASE_JAR generate -i $JSON_FILE -DinvokerPackage="com.knetikcloud.client",modelPackage="com.knetikcloud.model",apiPackage="com.knetikcloud.api" -l java $ID_FLAGS --artifact-id knetikcloud-java-client --library jersey2 -o sdk/java
cd sdk/java

sed -i -e 's~'"$README_ORIGINAL"'~'"$README_REPLACEMENT"'~g' README.md

git add -A
git commit -m "JSAPI Java API update"
git push -u origin master
cd ../..

#Javascript
mkdir -p sdk/javascript
chmod 777 sdk/javascript
cd sdk/javascript

git init
git config user.name "$GIT_USERNAME"
git config user.email "$GIT_EMAIL"
git remote add origin git@github.com:knetikmedia/knetikcloud-javascript-client.git
git pull origin master
rm -r *

cd ../..
java -jar $BASE_JAR generate -i $JSON_FILE -l javascript $ID_FLAGS --artifact-id knetikcloud-javascript-client -o sdk/javascript
cd sdk/javascript

sed -i -e 's~'"$README_ORIGINAL"'~'"$README_REPLACEMENT"'~g' README.md

git add -A
git commit -m "JSAPI Javascript API update"
git push -u origin master
cd ../..

#Objective C
mkdir -p sdk/objc
chmod 777 sdk/objc
cd sdk/objc

git init
git config user.name "$GIT_USERNAME"
git config user.email "$GIT_EMAIL"
git remote add origin git@github.com:knetikmedia/knetikcloud-objc-client.git
git pull origin master
rm -r *

cd ../..
java -jar $BASE_JAR generate -i $JSON_FILE -l objc -DpodName="JSAPI",classPrefix="JSAPI",gitRepoUrl="https://github.com/knetikmedia/knetikcloud-objc-client" $ID_FLAGS --artifact-id knetikcloud-objc-client -o sdk/objc
cd sdk/objc

sed -i -e 's~'"$README_ORIGINAL"'~'"$README_REPLACEMENT"'~g' README.md

git add -A
git commit -m "JSAPI Objective C API update"
git push -u origin master
cd ../..

#Perl
mkdir -p sdk/perl
chmod 777 sdk/perl
cd sdk/perl

git init
git config user.name "$GIT_USERNAME"
git config user.email "$GIT_EMAIL"
git remote add origin git@github.com:knetikmedia/knetikcloud-perl-client.git
git pull origin master
rm -r *

cd ../..
java -jar $BASE_JAR generate -i $JSON_FILE -l perl -DmoduleName="KnetikCloud" $ID_FLAGS --artifact-id knetikcloud-perl-client -o sdk/perl
cd sdk/perl

sed -i -e 's~'"$README_ORIGINAL_PERL"'~'"$README_REPLACEMENT"'~g' README.md

git add -A
git commit -m "JSAPI Perl API update"
git push -u origin master
cd ../..

#PHP
mkdir -p sdk/php
chmod 777 sdk/php
cd sdk/php

git init
git config user.name "$GIT_USERNAME"
git config user.email "$GIT_EMAIL"
git remote add origin git@github.com:knetikmedia/knetikcloud-php-client.git
git pull origin master
rm -r *

cd ../..
java -jar $BASE_JAR generate -i $JSON_FILE -l php -DcomposerVendorName="knetikmedia",composerProjectName="knetikcloud-php-client",invokerPackage="KnetikCloud",packagePath="KnetikCloudClient" $ID_FLAGS --artifact-id knetikcloud-php-client -o sdk/php
cd sdk/php

cd KnetikCloudClient
sed -i -e 's~'"$README_ORIGINAL"'~'"$README_REPLACEMENT"'~g' README.md
cd ..

git add -A
git commit -m "JSAPI PHP API update"
git push -u origin master
cd ../..

#Python
mkdir -p sdk/python
chmod 777 sdk/python
cd sdk/python

git init
git config user.name "$GIT_USERNAME"
git config user.email "$GIT_EMAIL"
git remote add origin git@github.com:knetikmedia/knetikcloud-python-client.git
git pull origin master
rm -r *

cd ../..
java -jar $BASE_JAR generate -i $JSON_FILE -l python -DpackageName="knetik_cloud" $ID_FLAGS --artifact-id knetikcloud-python-client -o sdk/python
cd sdk/python

sed -i -e 's~'"$README_ORIGINAL"'~'"$README_REPLACEMENT"'~g' README.md

git add -A
git commit -m "JSAPI Python API update"
git push -u origin master
cd ../..



#QT5cpp
#mkdir -p sdk/qt5cpp
#chmod 777 sdk/qt5cpp
#cd sdk/qt5cpp

#git init
#git config user.name "$GIT_USERNAME"
#git config user.email "$GIT_EMAIL"
#git remote add origin git@github.com:knetikmedia/knetikcloud-qt5cpp-client.git
#git pull origin master
#git rm -r *
#git commit -m "JSAPI QT5cpp API clear"
#git push -u origin master

#cd ../..
#java -jar $BASE_JAR generate -i $JSON_FILE -l qt5cpp $ID_FLAGS --artifact-id knetikcloud-qt5cpp-client -o sdk/qt5cpp
#cd sdk/qt5cpp

#git init
#git config user.name "$GIT_USERNAME"
#git config user.email "$GIT_EMAIL"
#git add .
#git commit -m "JSAPI QT5cpp API update"
#git push -u origin master
##cd ../..



#Ruby
mkdir -p sdk/ruby
chmod 777 sdk/ruby
cd sdk/ruby

git init
git config user.name "$GIT_USERNAME"
git config user.email "$GIT_EMAIL"
git remote add origin git@github.com:knetikmedia/knetikcloud-ruby-client.git
git pull origin master
rm -r *

cd ../..
java -jar $BASE_JAR generate -i $JSON_FILE -l ruby -DgemName="knetikcloud_client",moduleName="KnetikCloudClient",gemHomepage="http://knetikcloud.com" $ID_FLAGS --artifact-id knetikcloud-ruby-client -o sdk/ruby
cd sdk/ruby

sed -i -e 's~'"$README_ORIGINAL"'~'"$README_REPLACEMENT"'~g' README.md

git add -A
git commit -m "JSAPI Ruby API update"
git push -u origin master
cd ../..

#Scala
mkdir -p sdk/scala
chmod 777 sdk/scala
cd sdk/scala

git init
git config user.name "$GIT_USERNAME"
git config user.email "$GIT_EMAIL"
git remote add origin git@github.com:knetikmedia/knetikcloud-scala-client.git
git pull origin master
rm -r *

cd ../..
java -jar $BASE_JAR generate -i $JSON_FILE -l scala -DmodelPackage="com.knetikcloud.client.model",apiPackage="com.knetikcloud.client.model" $ID_FLAGS --artifact-id knetikcloud-scala-client -o sdk/scala
cd sdk/scala

git add -A
git commit -m "JSAPI Scala API update"
git push -u origin master
cd ../..

#Spring
mkdir -p sdk/spring
chmod 777 sdk/spring
cd sdk/spring

git init
git config user.name "$GIT_USERNAME"
git config user.email "$GIT_EMAIL"
git remote add origin git@github.com:knetikmedia/knetikcloud-spring-server.git
git pull origin master
rm -r *

cd ../..
java -jar $BASE_JAR generate -i $JSON_FILE -l spring -DmodelPackage="com.knetikcloud.model",apiPackage="com.knetikcloud.api" $ID_FLAGS --artifact-id knetikcloud-spring-server -o sdk/spring
cd sdk/spring

git add -A
git commit -m "JSAPI Spring API update"
git push -u origin master
cd ../..

#Swift
mkdir -p sdk/swift
chmod 777 sdk/swift
cd sdk/swift

git init
git config user.name "$GIT_USERNAME"
git config user.email "$GIT_EMAIL"
git remote add origin git@github.com:knetikmedia/knetikcloud-swift-client.git
git pull origin master
rm -r *

cd ../..
java -jar $BASE_JAR generate -i $JSON_FILE -l swift -DprojectName="JSAPI" $ID_FLAGS --artifact-id knetikcloud-swift-client -o sdk/swift
cd sdk/swift

git add -A
git commit -m "JSAPI Swift API update"
git push -u origin master
cd ../..

#Swift3
mkdir -p sdk/swift3
chmod 777 sdk/swift3
cd sdk/swift3

git init
git config user.name "$GIT_USERNAME"
git config user.email "$GIT_EMAIL"
git remote add origin git@github.com:knetikmedia/knetikcloud-swift3-client.git
git pull origin master
rm -r *

cd ../..
java -jar $BASE_JAR generate -i $JSON_FILE -l swift3 -DprojectName="JSAPI" $ID_FLAGS --artifact-id knetikcloud-swift3-client -o sdk/swift3
cd sdk/swift3

git add -A
git commit -m "JSAPI Swift3 API update"
git push -u origin master
cd ../..

#Tizen
mkdir -p sdk/tizen
chmod 777 sdk/tizen
cd sdk/tizen

git init
git config user.name "$GIT_USERNAME"
git config user.email "$GIT_EMAIL"
git remote add origin git@github.com:knetikmedia/knetikcloud-tizen-client.git
git pull origin master
rm -r *

cd ../..
java -jar $BASE_JAR generate -i $JSON_FILE -l tizen $ID_FLAGS --artifact-id knetikcloud-tizen-client -o sdk/tizen
cd sdk/tizen

git add -A
git commit -m "JSAPI Tizen API update"
git push -u origin master
cd ../..

#Typescript-Angular
mkdir -p sdk/typescript-angular
chmod 777 sdk/typescript-angular
cd sdk/typescript-angular

git init
git config user.name "$GIT_USERNAME"
git config user.email "$GIT_EMAIL"
git remote add origin git@github.com:knetikmedia/knetikcloud-typescript-angular-client.git
git pull origin master
rm -r *

cd ../..
java -jar $BASE_JAR generate -i $JSON_FILE -l typescript-angular $ID_FLAGS --artifact-id knetikcloud-typescript-angular-client -o sdk/typescript-angular
cd sdk/typescript-angular

git add -A
git commit -m "JSAPI Typescript-Angular API update"
git push -u origin master
cd ../..

#Typescript-Angular2
mkdir -p sdk/typescript-angular2
chmod 777 sdk/typescript-angular2
cd sdk/typescript-angular2

git init
git config user.name "$GIT_USERNAME"
git config user.email "$GIT_EMAIL"
git remote add origin git@github.com:knetikmedia/knetikcloud-typescript-angular2-client.git
git pull origin master
rm -r *

cd ../..
java -jar $BASE_JAR generate -i $JSON_FILE -l typescript-angular2 $ID_FLAGS --artifact-id knetikcloud-typescript-angular2-client -o sdk/typescript-angular2
cd sdk/typescript-angular2

git add -A
git commit -m "JSAPI Typescript-Angular2 API update"
git push -u origin master
cd ../..

#Typescript-Fetch
mkdir -p sdk/typescript-fetch
chmod 777 sdk/typescript-fetch
cd sdk/typescript-fetch

git init
git config user.name "$GIT_USERNAME"
git config user.email "$GIT_EMAIL"
git remote add origin git@github.com:knetikmedia/knetikcloud-typescript-fetch-client.git
git pull origin master
rm -r *

cd ../..
java -jar $BASE_JAR generate -i $JSON_FILE -l typescript-fetch $ID_FLAGS --artifact-id knetikcloud-typescript-fetch-client -o sdk/typescript-fetch
cd sdk/typescript-fetch

git add -A
git commit -m "JSAPI Typescript-Fetch API update"
git push -u origin master
cd ../..

#Typescript-Node
mkdir -p sdk/typescript-node
chmod 777 sdk/typescript-node
cd sdk/typescript-node

git init
git config user.name "$GIT_USERNAME"
git config user.email "$GIT_EMAIL"
git remote add origin git@github.com:knetikmedia/knetikcloud-typescript-node-client.git
git pull origin master
rm -r *

cd ../..
java -jar $BASE_JAR generate -i $JSON_FILE -l typescript-node $ID_FLAGS --artifact-id knetikcloud-typescript-node-client -o sdk/typescript-node
cd sdk/typescript-node

git add -A
git commit -m "JSAPI Typescript-Node API update"
git push -u origin master
cd ../..

#RESPONSE="$(curl http://jsapi3-integration.knetik.com/activities)"
#echo $RESPONSE
#echo "Here is a string" | grep -o -P '(?<=Here).*(?=string)'
#echo "Here we go"
#GARBAGE="$("$RESPONSE" | grep -o -P '(?<=request_id).*(?=})')"
#GARBAGE="$("Here is a string" | grep -o -P '(?<=Here).*(?=string)')"
#echo "RESULT!"
#echo $GARBAGE