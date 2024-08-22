package com.apicatalog.linkedtree.jsonld.primitive;

import java.util.Map;

import jakarta.json.JsonObjectBuilder;
import jakarta.json.JsonValue;

public class JsonLdMeta  {

    protected Map<String, JsonValue> value;
    
    public JsonLdMeta(Map<String, JsonValue> value) {
        this.value = value;
    }
    
    public Map<String, JsonValue> value() {
        return value;
    }

    public void write(JsonObjectBuilder builder) {
        value.entrySet().forEach(e -> builder.add(e.getKey(), e.getValue()));
        
    }
    
}
