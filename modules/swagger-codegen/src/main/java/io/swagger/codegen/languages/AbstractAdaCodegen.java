package io.swagger.codegen.languages;

import io.swagger.codegen.CodegenConfig;
import io.swagger.codegen.CodegenProperty;
import io.swagger.codegen.DefaultCodegen;
import io.swagger.models.properties.Property;

import java.util.Arrays;

abstract public class AbstractAdaCodegen extends DefaultCodegen implements CodegenConfig {

    public AbstractAdaCodegen() {
        super();

        /*
         * Reserved words.  Override this with reserved words specific to your language
         */
        setReservedWordsLowerCase(
                Arrays.asList(
                        "abort",
                        "abs",
                        "abstract",
                        "accept",
                        "access",
                        "aliased",
                        "all",
                        "and",
                        "array",
                        "at",
                        "begin",
                        "body",
                        "case",
                        "constant",
                        "declare",
                        "delay",
                        "digits",
                        "do",
                        "else",
                        "elsif",
                        "end",
                        "entry",
                        "exception",
                        "exit",
                        "for",
                        "function",
                        "generic",
                        "goto",
                        "if",
                        "in",
                        "interface",
                        "is",
                        "limited",
                        "loop",
                        "mod",
                        "new",
                        "not",
                        "null",
                        "of",
                        "or",
                        "others",
                        "out",
                        "overriding",
                        "package",
                        "pragma",
                        "private",
                        "procedure",
                        "protected",
                        "raise",
                        "range",
                        "record",
                        "rem",
                        "renames",
                        "requeue",
                        "return",
                        "reverse",
                        "select",
                        "separate",
                        "some",
                        "subtype",
                        "synchronized",
                        "tagged",
                        "task",
                        "terminate",
                        "then",
                        "type",
                        "until",
                        "use",
                        "when",
                        "while",
                        "with",
                        "xor")
        );
    }

    @Override
    public String toVarName(String name) {
        if (reservedWords.contains(name)) {
            return escapeReservedWord(name);
        }
        if (typeMapping.keySet().contains(name) || typeMapping.values().contains(name)
                || importMapping.values().contains(name) || defaultIncludes.contains(name)
                || languageSpecificPrimitives.contains(name)) {
            return sanitizeName(name);
        }

        if (name.length() > 1) {
            return sanitizeName(Character.toUpperCase(name.charAt(0)) + name.substring(1));
        }

        return sanitizeName(name);
    }

    @Override
    public String toParamName(String name) {
        return sanitizeName(super.toParamName(name));
    }

    @Override
    public CodegenProperty fromProperty(String name, Property p) {
        CodegenProperty property = super.fromProperty(name, p);
        String nameInCamelCase = property.nameInCamelCase;
        nameInCamelCase = sanitizeName(nameInCamelCase);
        property.nameInCamelCase = nameInCamelCase;
        return property;
    }
}
