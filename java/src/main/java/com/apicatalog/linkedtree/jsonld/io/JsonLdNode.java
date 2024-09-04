package com.apicatalog.linkedtree.jsonld.io;

import com.apicatalog.linkedtree.json.JsonUtils;

import jakarta.json.JsonValue;

public class JsonLdNode {

    public static boolean isTree(JsonValue value) {
        if (JsonUtils.isObject(value)) {
            
        } 
//        if (JsonUtils.isArray(value))
        return false;
    }
    
}
