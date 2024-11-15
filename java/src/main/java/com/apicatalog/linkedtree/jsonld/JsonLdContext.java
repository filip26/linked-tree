package com.apicatalog.linkedtree.jsonld;

import java.net.URI;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;

import com.apicatalog.linkedtree.json.JsonUtils;
import com.apicatalog.linkedtree.pi.ProcessingInstruction;

import jakarta.json.Json;
import jakarta.json.JsonObject;
import jakarta.json.JsonObjectBuilder;
import jakarta.json.JsonString;
import jakarta.json.JsonValue;

public record JsonLdContext(
        Collection<String> context) implements ProcessingInstruction {

    public static JsonObject set(final Collection<String> context, final JsonObject document) {

        if (context == null || context.isEmpty()) {
            return document;
        }

        JsonObjectBuilder builder = Json.createObjectBuilder();

        if (context.size() == 1) {
            builder.add(JsonLdKeyword.CONTEXT, context.iterator().next());

        } else {
            builder.add(JsonLdKeyword.CONTEXT, Json.createArrayBuilder(context));
        }

        document.entrySet().forEach(e -> builder.add(e.getKey(), e.getValue()));

        return builder.build();
    }

    /**
     * 
     * @param object
     * @return a collection of contexts attached to the {@link JsonObject}, never
     *         <code>null</code>
     * 
     * @throws IllegalArgumentException if type declaration is not an array of
     *                                  strings
     */
    public static Collection<String> strings(final JsonObject object) {
        return strings(object, Collections.emptyList());
    }

    public static Collection<String> strings(final JsonObject object, Collection<String> defaultValue) {

        final JsonValue contexts = object.get(JsonLdKeyword.CONTEXT);

        if (JsonUtils.isNull(contexts)) {
            return defaultValue;
        }

        final Collection<JsonValue> items = JsonUtils.toCollection(contexts);

        if (items.isEmpty()) {
            return defaultValue;
        }

        final List<String> strings = new ArrayList<>(items.size());

        for (final JsonValue context : items) {

            if (JsonUtils.isNotString(context)) {
                throw new IllegalArgumentException("Invalid context value type. Expected JsonString but got [" + context + "].");
            }

            final String contextUri = ((JsonString) context).getString();

            if (!isURI(contextUri)) {
                throw new IllegalArgumentException("Invalid context value. Expected URI but got [" + context + "].");
            }
            strings.add(contextUri);
        }
        return strings;
    }

    protected static final boolean isURI(final String value) {
        try {
            return URI.create(value) != null;
        } catch (IllegalArgumentException e) {
        }
        return false;
    }

}
