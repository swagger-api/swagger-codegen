# Prerequisites

If you're looking for the latest stable version, you can grab it directly from Maven.org (Java 8 runtime at a minimum):

```sh
# Download current stable 2.x.x branch (Swagger and OpenAPI version 2)
wget https://repo1.maven.org/maven2/io/swagger/swagger-codegen-cli/2.4.44/swagger-codegen-cli-2.4.44.jar -O swagger-codegen-cli.jar

java -jar swagger-codegen-cli.jar help

# Download current stable 3.x.x branch (OpenAPI version 3)
wget https://repo1.maven.org/maven2/io/swagger/codegen/v3/swagger-codegen-cli/3.0.65/swagger-codegen-cli-3.0.65.jar -O swagger-codegen-cli.jar

java -jar swagger-codegen-cli.jar --help
```

For Windows users, you will need to install [wget](http://gnuwin32.sourceforge.net/packages/wget.htm) or you can use Invoke-WebRequest in PowerShell (3.0+).

example:

```powershell
Invoke-WebRequest -OutFile swagger-codegen-cli.jar https://repo1.maven.org/maven2/io/swagger/swagger-codegen-cli/2.4.44/swagger-codegen-cli-2.4.44.jar
```

On a mac, it's even easier with `brew`:

```sh
brew install swagger-codegen
```

To build from source, you need the following installed and available in your `$PATH:`

- [Java 11+](http://java.oracle.com)

- [Apache maven 3.6.2 or greater](http://maven.apache.org/)

## OS X Users

Don't forget to install Java 11+.

Export `JAVA_HOME` in order to use the supported Java version:

```sh
export JAVA_HOME=`/usr/libexec/java_home -v 11`
export PATH=${JAVA_HOME}/bin:$PATH
```