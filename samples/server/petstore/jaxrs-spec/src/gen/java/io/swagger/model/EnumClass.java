package io.swagger.model;



import javax.xml.bind.annotation.XmlEnum;
import javax.xml.bind.annotation.XmlType;

@XmlType(name="EnumClass")
@XmlEnum
public enum EnumClass {
    {values&#x3D;[_abc, -efg, (xyz)], enumVars&#x3D;[{name&#x3D;_ABC, value&#x3D;&quot;_abc&quot;}, {name&#x3D;_EFG, value&#x3D;&quot;-efg&quot;}, {name&#x3D;_XYZ_, value&#x3D;&quot;(xyz)&quot;}]}, 
    
    public String value() {
        return name();
    }

    public static EnumClass fromValue(String v) {
        return valueOf(v);
    }
}

