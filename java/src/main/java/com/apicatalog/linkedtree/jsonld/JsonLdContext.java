package com.apicatalog.linkedtree.jsonld;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;

import com.apicatalog.linkedtree.json.JsonUtils;

import jakarta.json.JsonObject;
import jakarta.json.JsonString;
import jakarta.json.JsonValue;

public final class JsonLdContext {

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

        final JsonValue contexts = object.get(JsonLdKeyword.CONTEXT);

        if (JsonUtils.isNull(contexts)) {
            Collections.emptyList();
        }

        final Collection<JsonValue> items = JsonUtils.toCollection(contexts);

        if (items.isEmpty()) {
            Collections.emptyList();
        }

        final List<String> strings = new ArrayList<>(items.size());

        for (final JsonValue context : items) {
            if (JsonUtils.isNotString(context)) {
                throw new IllegalArgumentException("Invalid context value type. Expected JsonString but got [" + context + "].");
            }
            // TODO
//            if (UriUtils.isURI(((JsonString) context).getString())
//                    ) {
//              throw new IllegalArgumentException("Invalid context value. Expected URI but got [" + context + "].");
//            }

            final String contextUri = ((JsonString) context).getString();

            strings.add(contextUri);
        }

        return strings;
    }
}
