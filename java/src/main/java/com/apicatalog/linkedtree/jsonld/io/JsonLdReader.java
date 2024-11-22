package com.apicatalog.linkedtree.jsonld.io;

import java.net.URI;
import java.util.ArrayList;
import java.util.Collection;

import com.apicatalog.jsonld.JsonLd;
import com.apicatalog.jsonld.JsonLdError;
import com.apicatalog.jsonld.document.JsonDocument;
import com.apicatalog.jsonld.loader.DocumentLoader;
import com.apicatalog.linkedtree.adapter.NodeAdapterError;
import com.apicatalog.linkedtree.builder.TreeBuilderError;
import com.apicatalog.linkedtree.jsonld.JsonLdKeyword;
import com.apicatalog.linkedtree.orm.Context;
import com.apicatalog.linkedtree.orm.Fragment;
import com.apicatalog.linkedtree.orm.mapper.TreeReaderMapping;

import jakarta.json.Json;
import jakarta.json.JsonArray;
import jakarta.json.JsonObject;
import jakarta.json.JsonStructure;

public class JsonLdReader {

    protected final JsonLdTreeReader reader;
    protected DocumentLoader loader;

    protected JsonLdReader(JsonLdTreeReader reader, DocumentLoader loader) {
        this.reader = reader;
        this.loader = loader;
    }

    public static final JsonLdReader of(TreeReaderMapping mapping, DocumentLoader loader) {
        return of(JsonLdTreeReader.of(mapping), loader);
    }

    public static final JsonLdReader of(JsonLdTreeReader reader, DocumentLoader loader) {
        return new JsonLdReader(reader, loader);
    }

    public <T> T read(Class<T> typeInterface, JsonObject fragment) throws TreeBuilderError, NodeAdapterError {
        return read(typeInterface, fragment, null);
    }
    
    public <T> T read(Class<T> typeInterface, JsonObject fragment, URI base) throws TreeBuilderError, NodeAdapterError {

        JsonObject input = fragment;

        JsonStructure expandContext = null;

        // check context
        if (!input.containsKey(JsonLdKeyword.CONTEXT)) {
            Collection<String> contexts = context(typeInterface, new ArrayList<>(2)); // TODO get from scans, use cache
            // inject context if exist
            if (!contexts.isEmpty()) {
                expandContext = Json.createArrayBuilder(contexts).build();
            }
        }

        try {
            final JsonArray expanded = JsonLd
                    .expand(JsonDocument.of(fragment))
                    .context(expandContext)
                    .base(base)
                    .loader(loader)
                    .get();

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
