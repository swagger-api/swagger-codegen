#!/bin/bash
if [ $# -lt 2 ]; then
  echo "USAGE:  DataCollectorGenerate.sh swagger_doc_url version_number"
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

ID_FLAGS="--group-id io.knetik --artifact-version $VERSION_NUMBER"

mkdir -p sdk
chmod 777 sdk



#Android
mkdir -p sdk/android
chmod 777 sdk/android
cd sdk/android

git init
git config user.name "$GIT_USERNAME"
git config user.email "$GIT_EMAIL"
git remote add origin git@github.com:splytanalytics/splyt-android-client.git
git pull origin master
rm -r *

cd ../..
java -jar $BASE_JAR generate -i $JSON_FILE -l android $ID_FLAGS --artifact-id splyt-android-client -o sdk/android
cd sdk/android

#sed -i -e 's~'"$POM_ORIGINAL"'~'"$POM_REPLACEMENT"'~g' pom.xml
sed -i -e 's~'"$README_ORIGINAL"'~'"$README_REPLACEMENT"'~g' README.md

git add -A
git commit -m "JSAPI Android API update"
git push -u origin master
cd ../..



#CppRest
mkdir -p sdk/cpprest
chmod 777 sdk/cpprest
cd sdk/cpprest

git init
git config user.name "$GIT_USERNAME"
git config user.email "$GIT_EMAIL"
git remote add origin git@github.com:splytanalytics/splyt-cpprest-client.git
git pull origin master
rm -r *

cd ../..
java -jar $BASE_JAR generate -i $JSON_FILE -l cpprest -DmodelPackage="io.knetik.client.model",apiPackage="io.knetik.client.api" $ID_FLAGS --artifact-id splyt-cpprest-client -o sdk/cpprest
cd sdk/cpprest

#sed -i -e 's~'"$POM_ORIGINAL"'~'"$POM_REPLACEMENT"'~g' pom.xml
#sed -i -e 's~'"$README_ORIGINAL"'~'"$README_REPLACEMENT"'~g' README.md

git add -A
git commit -m "Splyt CppRest API update"
git push -u origin master
cd ../..


#Java
mkdir -p sdk/java
chmod 777 sdk/java
cd sdk/java

git init
git config user.name "$GIT_USERNAME"
git config user.email "$GIT_EMAIL"
git remote add origin git@github.com:splytanalytics/splyt-java-client.git
git pull origin master
rm -r *

cd ../..
java -jar $BASE_JAR generate -i $JSON_FILE -DinvokerPackage="io.knetik.client",modelPackage="io.knetik.model",apiPackage="io.knetik.api" -l java $ID_FLAGS --artifact-id splyt-java-client --library jersey2 -o sdk/java
cd sdk/java

sed -i -e 's~'"$README_ORIGINAL"'~'"$README_REPLACEMENT"'~g' README.md

git add -A
git commit -m "JSAPI Java API update"
git push -u origin master
cd ../..



#Objective C
mkdir -p sdk/objc
chmod 777 sdk/objc
cd sdk/objc

git init
git config user.name "$GIT_USERNAME"
git config user.email "$GIT_EMAIL"
git remote add origin git@github.com:splytanalytics/splyt-objc-client.git
git pull origin master
rm -r *

cd ../..
java -jar $BASE_JAR generate -i $JSON_FILE -l objc -DpodName="DataCollectorClient",classPrefix="DCC",gitRepoUrl="https://github.com/splytanalytics/splyt-objc-client" $ID_FLAGS --artifact-id splyt-objc-client -o sdk/objc
cd sdk/objc

#sed -i -e 's~'"$POM_ORIGINAL"'~'"$POM_REPLACEMENT"'~g' pom.xml
sed -i -e 's~'"$README_ORIGINAL"'~'"$README_REPLACEMENT"'~g' README.md

git add -A
git commit -m "JSAPI Objective C API update"
git push -u origin master
cd ../..



#QT5cpp
#mkdir -p sdk/qt5cpp
#chmod 777 sdk/qt5cpp
#cd sdk/qt5cpp

#git init
#git config user.name "$GIT_USERNAME"
#git config user.email "$GIT_EMAIL"
#git remote add origin git@github.com:splytanalytics/splyt-qt5cpp-client.git
#git pull origin master
#git rm -r *
#git commit -m "Splyt QT5cpp API clear"
#git push -u origin master

#cd ../..
#java -jar $BASE_JAR generate -i $JSON_FILE -l qt5cpp -DpodName="DataCollectorClient",classPrefix="DCC" $ID_FLAGS --artifact-id splyt-qt5cpp-client -o sdk/qt5cpp
#cd sdk/qt5cpp

#git init
#git config user.name "$GIT_USERNAME"
#git config user.email "$GIT_EMAIL"
#git add .
#git commit -m "Splyt QT5cpp API update"
#git push -u origin master
##cd ../..



#Swift
mkdir -p sdk/swift
chmod 777 sdk/swift
cd sdk/swift

git init
git config user.name "$GIT_USERNAME"
git config user.email "$GIT_EMAIL"
git remote add origin git@github.com:splytanalytics/splyt-swift-client.git
git pull origin master
rm -r *

cd ../..
java -jar $BASE_JAR generate -i $JSON_FILE -l swift -DprojectName="DataCollector" $ID_FLAGS --artifact-id splyt-swift-client -o sdk/swift
cd sdk/swift

#sed -i -e 's~'"$POM_ORIGINAL"'~'"$POM_REPLACEMENT"'~g' pom.xml

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
git remote add origin git@github.com:splytanalytics/splyt-swift3-client.git
git pull origin master
rm -r *

cd ../..
java -jar $BASE_JAR generate -i $JSON_FILE -l swift3 -DprojectName="DataCollector" $ID_FLAGS --artifact-id splyt-swift3-client -o sdk/swift3
cd sdk/swift3

#sed -i -e 's~'"$POM_ORIGINAL"'~'"$POM_REPLACEMENT"'~g' pom.xml

git add -A
git commit -m "JSAPI Swift3 API update"
git push -u origin master
cd ../..