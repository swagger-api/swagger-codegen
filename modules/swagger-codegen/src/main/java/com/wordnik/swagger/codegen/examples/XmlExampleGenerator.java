package com.wordnik.swagger.codegen.examples;


import com.wordnik.swagger.util.Json;
import com.wordnik.swagger.models.*;
import com.wordnik.swagger.models.properties.*;

import java.text.SimpleDateFormat;
import java.util.*;

public class XmlExampleGenerator {
  public static String NEWLINE = "\n";
  public static String TAG_START = "<";
  public static String CLOSE_TAG = ">";
  public static String TAG_END = "</";
  protected Map<String, Model> examples;
  protected SimpleDateFormat dtFormat = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSS'Z'");
  protected SimpleDateFormat dateFormat = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSS'Z'");

  public XmlExampleGenerator(Map<String, Model> examples) {
    this.examples = examples;
    if(examples == null)
      examples = new HashMap<String, Model>();
  }

  public String toXml(Property property) {
    return toXml(null, property, 0);
  }

  protected String toXml(Model model, int indent) {
    if(model instanceof RefModel) {
      RefModel ref = (RefModel) model;
      Model actualModel = examples.get(ref.getSimpleRef());
      if(actualModel instanceof ModelImpl)
        return modelImplToXml((ModelImpl)actualModel, indent);
    }
    else if(model instanceof ModelImpl) {
      return modelImplToXml((ModelImpl)model, indent);
    }
    return null;
  }

  protected String modelImplToXml(ModelImpl model, int indent) {
    StringBuilder sb = new StringBuilder();
    // attributes
    Map<String, Property> attributes = new LinkedHashMap<String, Property>();
    Map<String, Property> elements = new LinkedHashMap<String, Property>();

    String name = model.getName();
    String namespace;
    String prefix;
    Boolean wrapped;

    Xml xml = model.getXml();
    if(xml != null) {
      if(xml.getName() != null)
        name = xml.getName();
    }
    for(String pName : model.getProperties().keySet()) {
      Property p = model.getProperties().get(pName);
      if(p.getXml() != null && p.getXml().getAttribute())
        attributes.put(pName, p);
      else
        elements.put(pName, p);
    }
    sb.append(indent(indent)).append(TAG_START);
    sb.append(name);
    for(String pName : attributes.keySet()) {
      Property p = attributes.get(pName);
      sb.append(" ").append(pName).append("=").append(quote(toXml(null, p, 0)));
    }
    sb.append(CLOSE_TAG);
    sb.append(NEWLINE);
    for(String pName : elements.keySet()) {
      Property p = elements.get(pName);
      sb.append(toXml(pName, p, indent + 1));
      sb.append(NEWLINE);
    }
    sb.append(indent(indent)).append(TAG_END).append(name).append(CLOSE_TAG);

    return sb.toString();
  }

  protected String quote(String string) {
    return "\"" + string + "\"";
  }

  protected String toXml(String name, Property property, int indent) {
    if(property == null) {
      return "";
    }
    StringBuilder sb = new StringBuilder();
    if(property.getXml() != null) {

    }
    if(property instanceof IntegerProperty) {
      IntegerProperty p = (IntegerProperty) property;
      if(name != null) {
        sb.append(indent(indent));
        sb.append(openTag(name));
      }
      if(property.getExample() != null)
        sb.append(property.getExample());
      else
        sb.append("0");
      if(name != null)
        sb.append(closeTag(name));
    }
    else if(property instanceof BooleanProperty) {
      BooleanProperty p = (BooleanProperty) property;
      if(name != null) {
        sb.append(indent(indent));
        sb.append(openTag(name));
      }
      if(property.getExample() != null)
        sb.append(property.getExample());
      else
        sb.append("true");
      if(name != null)
        sb.append(closeTag(name));
    }
    else if(property instanceof LongProperty) {
      LongProperty p = (LongProperty) property;
      if(name != null) {
        sb.append(indent(indent));
        sb.append(openTag(name));
      }
      if(property.getExample() != null)
        sb.append(property.getExample());
      else
        sb.append("123456");
      if(name != null)
        sb.append(closeTag(name));
    }
    else if(property instanceof DateProperty) {
      DateProperty p = (DateProperty) property;
      if(name != null) {
        sb.append(indent(indent));
        sb.append(openTag(name));
      }
      if(property.getExample() != null)
        sb.append(property.getExample());
      else
        sb.append(dateFormat.format(new Date()));
      if(name != null)
        sb.append(closeTag(name));
    }
    else if(property instanceof DateTimeProperty) {
      DateTimeProperty p = (DateTimeProperty) property;
      if(name != null) {
        sb.append(indent(indent));
        sb.append(openTag(name));
      }
      if(property.getExample() != null)
        sb.append(property.getExample());
      else
        sb.append(dtFormat.format(new Date()));
      if(name != null)
        sb.append(closeTag(name));
    }
    else if(property instanceof StringProperty) {
      StringProperty p = (StringProperty) property;
      if(name != null) {
        sb.append(indent(indent));
        sb.append(openTag(name));
      }
      if(property.getExample() != null)
        sb.append(property.getExample());
      else
        sb.append("string");
      if(name != null)
        sb.append(closeTag(name));
    }
    else if(property instanceof ArrayProperty) {
      ArrayProperty p = (ArrayProperty) property;
      Property inner = p.getItems();
      boolean wrapped = false;
      if(property.getXml() != null && property.getXml().getWrapped())
        wrapped = true;
      if(wrapped) {
        if(name != null) {
          sb.append(indent(indent));
          sb.append(openTag(name + "s"));
        }
        if(name != null)
          sb.append(closeTag(name + "s"));
      }
      else
        sb.append(toXml(name, inner, indent));
    }
    else if(property instanceof RefProperty) {
      RefProperty ref = (RefProperty) property;
      Model actualModel = examples.get(ref.getSimpleRef());
      sb.append(toXml(actualModel, indent));
    }
    else {
      System.out.println(property);
    }
    return sb.toString();
  }

  protected String openTag(String name) {
    return "<" + name + ">";
  }

  protected String closeTag(String name) {
    return "</" + name + ">";
  }

  protected String indent(int indent) {
    StringBuffer sb = new StringBuffer();
    for(int i = 0; i < indent; i++) {
      sb.append("  ");
    }
    return sb.toString();
  }
}