package io.swagger.codegen;

import io.swagger.models.ExternalDocs;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;


public class CodegenModel {
    public String parent, parentSchema;
    public List<String> interfaces;

    // References to parent and interface CodegenModels. Only set when code generator supports inheritance.
    public CodegenModel parentModel;
    public List<CodegenModel> interfaceModels;

    public String name, classname, description, classVarName, modelJson, dataType;
    public String classFilename; // store the class file name, mainly used for import
    public String unescapedDescription;
    public String discriminator;
    public String defaultValue;
    public String arrayModelType;
    public List<CodegenProperty> vars = new ArrayList<CodegenProperty>();
    public List<CodegenProperty> requiredVars = new ArrayList<CodegenProperty>(); // a list of required properties
    public List<CodegenProperty> optionalVars = new ArrayList<CodegenProperty>(); // a list of optional properties
    public List<CodegenProperty> readOnlyVars = new ArrayList<CodegenProperty>(); // a list of read-only properties
    public List<CodegenProperty> readWriteVars = new ArrayList<CodegenProperty>(); // a list of properties for read, write
    public List<CodegenProperty> allVars;
    public List<CodegenProperty> parentVars = new ArrayList<>();
    public Map<String, Object> allowableValues;

    // Sorted sets of required parameters.
    public Set<String> mandatory = new TreeSet<String>();
    public Set<String> allMandatory;

    public Set<String> imports = new TreeSet<String>();
    public Boolean hasVars, emptyVars, hasMoreModels, hasEnums, isEnum, hasRequired, isArrayModel, hasChildren;
    public Boolean hasOnlyReadOnly = true; // true if all properties are read-only
    public ExternalDocs externalDocs;

    public Map<String, Object> vendorExtensions;

    //The type of the value from additional properties. Used in map like objects.
    public String additionalPropertiesType;

    {
        // By default these are the same collections. Where the code generator supports inheritance, composed models
        // store the complete closure of owned and inherited properties in allVars and allMandatory.
        allVars = vars;
        allMandatory = mandatory;
    }

    @Override
    public String toString() {
        return String.format("%s(%s)", name, classname);
    }

    @Override
    public boolean equals(Object obj) {
        return EqualsBuilder.reflectionEquals(this, obj);
    }

    @Override
    public int hashCode() {
        return HashCodeBuilder.reflectionHashCode(this);
    }
}
