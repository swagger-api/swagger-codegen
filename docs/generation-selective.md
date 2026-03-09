# Selective generation

You may not want to generate *all* models in your project.  Likewise you may want just one or two apis to be written. If that's the case, you can use system properties to control the output:

The default is generate *everything* supported by the specific library.  Once you enable a feature, it will restrict the contents generated:

```sh
# generate only models
java -Dmodels {opts}

# generate only apis
java -Dapis {opts}

# generate only supporting files
java -DsupportingFiles

# generate models and supporting files
java -Dmodels -DsupportingFiles
```

To control the specific files being generated, you can pass a CSV list of what you want:

```sh
# generate the User and Pet models only
-Dmodels=User,Pet

# generate the User model and the supportingFile `StringUtil.java`:
-Dmodels=User -DsupportingFiles=StringUtil.java
```

To control generation of docs and tests for api and models, pass false to the option. For api, these options are  `-DapiTests=false` and `-DapiDocs=false`. For models, `-DmodelTests=false` and `-DmodelDocs=false`.
These options default to true and don't limit the generation of the feature options listed above (like `-Dapi`):

```sh
# generate only models (with tests and documentation)
java -Dmodels {opts}

# generate only models (with tests but no documentation)
java -Dmodels -DmodelDocs=false {opts}

# generate only User and Pet models (no tests and no documentation)
java -Dmodels=User,Pet -DmodelTests=false {opts}

# generate only apis (without tests)
java -Dapis -DapiTests=false {opts}

# generate only apis (modelTests option is ignored)
java -Dapis -DmodelTests=false {opts}
```

When using selective generation, _only_ the templates needed for the specific generation will be used.
