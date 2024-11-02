package com.apicatalog.linkedtree.jsonld.io;

import java.util.ArrayList;
import java.util.Collection;

import com.apicatalog.jsonld.JsonLd;
import com.apicatalog.jsonld.JsonLdError;
import com.apicatalog.jsonld.document.JsonDocument;
import com.apicatalog.linkedtree.adapter.NodeAdapterError;
import com.apicatalog.linkedtree.builder.TreeBuilderError;
import com.apicatalog.linkedtree.jsonld.JsonLdKeyword;
import com.apicatalog.linkedtree.orm.Context;
import com.apicatalog.linkedtree.orm.Fragment;
import com.apicatalog.linkedtree.orm.mapper.TreeMapping;

import jakarta.json.Json;
import jakarta.json.JsonArray;
import jakarta.json.JsonObject;
import jakarta.json.JsonStructure;

public class JsonLdReader {

    final JsonLdTreeReader reader;
    
    protected JsonLdReader(JsonLdTreeReader reader) {
        this.reader = reader;
    }
    
    public static final JsonLdReader of(TreeMapping mapping) {
        return new JsonLdReader(JsonLdTreeReader.of(mapping));
    }
    
    public <T> T read(Class<T> typeInterface, JsonObject fragment) throws TreeBuilderError, NodeAdapterError {
        
        JsonObject input = fragment;

        JsonStructure expandContext = null;
        
        // check context
        if (!input.containsKey(JsonLdKeyword.CONTEXT)) {
            Collection<String> contexts = context(typeInterface, new ArrayList<>(2));
            if (!contexts.isEmpty()) {
                expandContext = Json.createArrayBuilder(contexts).build();
            }
        }

        try {
            JsonArray expanded = JsonLd.expand(JsonDocument.of(fragment)).context(expandContext).get();
            
            return reader.read(typeInterface, expanded);
            
        } catch (JsonLdError e) {
            throw new NodeAdapterError(e);
        }
    }

    Collection<String> context(Class<?> typeInterface, Collection<String> context) {
        if (typeInterface == null) {
            return context;
        }

        if (typeInterface.getInterfaces() != null) {
            for (Class<?> superType : typeInterface.getInterfaces()) {
                context(superType, context);
            }
        }

        Fragment fragment = typeInterface.getDeclaredAnnotation(Fragment.class);

        if (fragment != null) {

            Context fragmentContext = typeInterface.getAnnotation(Context.class);
            if (fragmentContext != null) {
                if (fragmentContext.override()) {
                    context.clear();
                }
                for (String ctx : fragmentContext.value()) {
                    context.add(ctx);
                }
            }
        }
        return context;
    }
    
}
