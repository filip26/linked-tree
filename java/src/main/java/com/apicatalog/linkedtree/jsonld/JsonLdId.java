package com.apicatalog.linkedtree.jsonld;

import java.net.URI;

import com.apicatalog.linkedtree.json.JsonUtils;

import jakarta.json.JsonObject;
import jakarta.json.JsonString;
import jakarta.json.JsonValue;

/**
 * Utility class to manipulate with JSON-LD id in an expanded JSON-LD
 * document.
 */
public final class JsonLdId {

    public static URI uri(final JsonObject object) {

        final JsonValue json = object.get(JsonLdKeyword.ID);

        if (json != null) {
        if (JsonUtils.isString(json)) {
                return URI.create(((JsonString)json).getString()); 
        }

        throw new IllegalArgumentException("An invalid @id. Expected URI but got [" + json + "].");
        }
        return null;
    }
    
    public static String string(final JsonObject object) {

        final JsonValue json = object.get(JsonLdKeyword.ID);

        if (json != null) {
            if (JsonUtils.isString(json)) {
                    return ((JsonString)json).getString(); 
            }
    
            throw new IllegalArgumentException("An invalid @id. Expected URI but got [" + json + "].");
        }
        return null;
    }

}
