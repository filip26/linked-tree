package com.apicatalog.linkedtree.jsonld.io;

import java.util.Map;

import jakarta.json.JsonObjectBuilder;
import jakarta.json.JsonValue;

public record JsonLdMeta(Map<String, JsonValue> value)  {

    public void write(JsonObjectBuilder builder) {
        value.entrySet().forEach(e -> builder.add(e.getKey(), e.getValue()));        
    }
    
}
