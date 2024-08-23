package com.apicatalog.linkedtree.jsonld;

import java.util.ArrayList;
import java.util.List;
import java.util.Map.Entry;
import java.util.Objects;

import com.apicatalog.linkedtree.json.JsonUtils;

import jakarta.json.JsonArray;
import jakarta.json.JsonObject;
import jakarta.json.JsonValue;

/**
 *
 * @see <a href="https://w3c.github.io/json-ld-api/tests/#json-ld-object-comparison">JSON-LD Object comparison</a>
 *
 */
final class JsonLdComparison {

    private JsonLdComparison() {
    }

    public static final boolean equals(final JsonValue value1, final JsonValue value2) {

        return equals(value1, value2, null);
    }

    private static final boolean equals(final JsonValue value1, final JsonValue value2, final String parentProperty) {

        if (JsonUtils.isNull(value1) && JsonUtils.isNull(value2)) {
            return true;
        }

        if (JsonUtils.isScalar(value1) && JsonUtils.isScalar(value2)) {
            return Objects.equals(value1, value2);
        }

        if (JsonUtils.isArray(value1) && JsonUtils.isArray(value2)) {
            return arrayEquals(value1.asJsonArray(), value2.asJsonArray(), parentProperty);
        }

        if (JsonUtils.isObject(value1) && JsonUtils.isObject(value2)) {
            return objectEquals(value1.asJsonObject(), value2.asJsonObject());
        }

        return false;
    }

    private static final boolean objectEquals(final JsonObject object1, final JsonObject object2) {
        if (object1.size() != object2.size()) {
            return false;
        }

        for (final Entry<String, JsonValue> entry1 : object1.entrySet()) {

            if (!object2.containsKey(entry1.getKey())) {
                return false;
            }

            if (!equals(entry1.getValue(), object2.get(entry1.getKey()), entry1.getKey())) {
                return false;
            }
        }
        return true;
    }

    private static final boolean arrayEquals(final JsonArray array1, final JsonArray array2, final String parentProperty) {

        if (array1.size() != array2.size()) {
            return false;
        }

        if (array1.isEmpty()) {
            return true;
        }

        // For values of @list, the order of these items is significant
        if ("@list".equals(parentProperty)) {

            for (int i=0; i < array1.size(); i++) {
                if (!equals(array1.get(i), array2.get(i))) {
                    return false;
                }
            }

            return true;
        }

        return arraysEqualsUnordered(array1, array2);
    }

    // JSON arrays are generally compared without regard to order
    private static final boolean arraysEqualsUnordered(final JsonArray array1, final JsonArray array2) {

        if (array1.size() != array2.size()) {
            return false;
        }

        if (array1.isEmpty()) {
            return true;
        }

        final List<JsonValue> remaining = new ArrayList<>(array2);

        for (final JsonValue item1 : array1) {

            boolean found = false;

            for (final JsonValue item2 : remaining) {

                found = equals(item1, item2);

                if (found) {
                    remaining.remove(item2);
                    break;
                }
            }

            if (!found) {
                return false;
            }
        }

        return remaining.isEmpty();
    }
}
