package io.swagger.model;



import javax.xml.bind.annotation.XmlEnum;
import javax.xml.bind.annotation.XmlType;

@XmlType(name="OuterEnum")
@XmlEnum
public enum OuterEnum {
    {values&#x3D;[placed, approved, delivered], enumVars&#x3D;[{name&#x3D;PLACED, value&#x3D;&quot;placed&quot;}, {name&#x3D;APPROVED, value&#x3D;&quot;approved&quot;}, {name&#x3D;DELIVERED, value&#x3D;&quot;delivered&quot;}]}, 
    
    public String value() {
        return name();
    }

    public static OuterEnum fromValue(String v) {
        return valueOf(v);
    }
}

