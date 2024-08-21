package com.apicatalog.linkedtree.json;

import java.util.Collection;

import com.apicatalog.linkedtree.LinkedData;
import com.apicatalog.linkedtree.LinkedFragment;
import com.apicatalog.linkedtree.LinkedTree;
import com.apicatalog.linkedtree.LinkedValue;

import jakarta.json.Json;
import jakarta.json.JsonArray;
import jakarta.json.JsonArrayBuilder;
import jakarta.json.JsonObject;
import jakarta.json.JsonObjectBuilder;
import jakarta.json.JsonValue;

public class JsonTreeWriter {

    
    public JsonArray write(LinkedTree tree) {
        
        final JsonArrayBuilder builder = Json.createArrayBuilder();
        
        for (final LinkedFragment fragment : tree.fragments()) {
            builder.add(writeFragment(fragment));
        }
        
        return builder.build();
    }
    
    
    public JsonObject writeFragment(LinkedFragment fragment) {
        
        final JsonObjectBuilder builder = Json.createObjectBuilder();
        
        if (fragment.id() != null) {
            builder.add("@id", fragment.id().uri().toString());
        }
        if (fragment.type() != null) {
            
        }
        for (final String term : fragment.terms()) {
            
            final JsonArrayBuilder termValues = Json.createArrayBuilder();
            
            for (final LinkedData data : fragment.data(term)) {
                termValues.add(writeData(data));
            }
            
            builder.add(term, termValues);
        }
        
        return builder.build();
    }
    
    public JsonValue writeData(LinkedData data) {
        
        if (data == null) {
            return JsonValue.NULL;
        }
        
        if (data.isNode()) {
            return writeFragment(data.asNode());
        }
        
        if (data.isValue()) {
            return writeLiteral(data.asValue());
        }
        
        if (data.isTree()) {
            return write(data.asTree());
        }
        
        throw new IllegalStateException();
    }
    
    public JsonValue writeLiteral(LinkedValue value) {
        
        final JsonObjectBuilder builder = Json.createObjectBuilder();
        
        builder.add("@value", value.value());
        return builder.build();
    }
}
