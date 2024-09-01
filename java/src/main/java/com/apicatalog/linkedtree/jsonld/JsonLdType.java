package com.apicatalog.linkedtree.jsonld;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;

import com.apicatalog.linkedtree.json.JsonUtils;

import jakarta.json.JsonObject;
import jakarta.json.JsonString;
import jakarta.json.JsonValue;

/**
 * Utility class to manipulate with JSON-LD type in an expanded JSON-LD
 * document.
 */
public final class JsonLdType {

    /**
     * 
     * @param object
     * @return a collection of types attached to the {@link JsonObject}, never
     *         <code>null</code>
     * 
     * @throws IllegalArgumentException if type declaration is not an array of
     *                                  strings
     */
    public static Collection<String> strings(final JsonObject object) {

        final JsonValue jsonTypes = object.get(JsonLdKeyword.TYPE);

        if (JsonUtils.isArray(jsonTypes)) {

            if (jsonTypes.asJsonArray().isEmpty()) {
                return Collections.emptyList();
            }

            final List<String> types = new ArrayList<>(jsonTypes.asJsonArray().size());

            for (final JsonValue jsonType : jsonTypes.asJsonArray()) {
                if (JsonUtils.isNotString(jsonType)) {
                    throw new IllegalArgumentException("An invalid @type. Expected a string value but got [" + jsonType + "].");
                }

                final String type = ((JsonString) jsonType).getString();

                // TODO UriUtils.isURI(type)

                types.add(type);
            }
            return types;

        } else if (JsonUtils.isNull(jsonTypes)) {
            return Collections.emptySet();
        }

        throw new IllegalArgumentException("An invalid @type. Expected an array of strings but got [" + jsonTypes + "].");
    }

    public boolean hasType(final String type, final JsonObject object) {
        if (JsonUtils.isNull(object)) {
            return false;
        }

        final JsonValue types = object.get(JsonLdKeyword.TYPE);

        return JsonUtils.isNotNull(types) && JsonUtils.toStream(types)
                .filter(JsonUtils::isString)
                .map(t -> ((JsonString) t).getString())
                .anyMatch(t -> type.equals(t));
    }
}
