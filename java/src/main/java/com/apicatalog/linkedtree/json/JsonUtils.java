package com.apicatalog.linkedtree.json;

import java.util.Collection;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.stream.Stream;

import jakarta.json.Json;
import jakarta.json.JsonArray;
import jakarta.json.JsonObject;
import jakarta.json.JsonObjectBuilder;
import jakarta.json.JsonString;
import jakarta.json.JsonValue;
import jakarta.json.JsonValue.ValueType;

public final class JsonUtils {

    public static final boolean contains(String text, JsonValue value) {

        if (text == null) {
            return value == null;
        }

        if (value == null) {
            return false;
        }

        if (JsonUtils.isString(value)) {
            return text.equals(((JsonString) value).getString());
        }

        if (JsonUtils.isArray(value)) {
            return value.asJsonArray().contains(Json.createValue(text));
        }
        if (JsonUtils.isObject(value)) {
            return value.asJsonObject().containsKey(text);
        }

        return false;
    }

    public static final boolean containsKey(JsonValue object, String key) {
        return object != null
                && ValueType.OBJECT.equals(object.getValueType())
                && object.asJsonObject().containsKey(key);
    }

    public static final boolean isScalar(final JsonValue value) {
        return value != null
                && !ValueType.ARRAY.equals(value.getValueType())
                && !ValueType.OBJECT.equals(value.getValueType());
    }

    public static final boolean isNotScalar(final JsonValue value) {
        return !isScalar(value);
    }

    public static final boolean isNull(final JsonValue value) {
        return value == null || ValueType.NULL.equals(value.getValueType());
    }

    public static final boolean isNotNull(final JsonValue value) {
        return !isNull(value);
    }

    public static boolean isString(JsonValue value) {
        return value != null && ValueType.STRING.equals(value.getValueType());
    }

    public static boolean isNotString(JsonValue value) {
        return value == null || !ValueType.STRING.equals(value.getValueType());
    }

    public static boolean isNotArray(JsonValue value) {
        return value == null || !ValueType.ARRAY.equals(value.getValueType());
    }

    public static boolean isArray(JsonValue value) {
        return value != null && ValueType.ARRAY.equals(value.getValueType());
    }

    public static boolean isObject(JsonValue value) {
        return value != null && ValueType.OBJECT.equals(value.getValueType());
    }

    public static boolean isNotObject(JsonValue value) {
        return value == null || !ValueType.OBJECT.equals(value.getValueType());
    }

    public static boolean isNumber(JsonValue value) {
        return value != null && ValueType.NUMBER.equals(value.getValueType());
    }

    public static boolean isNotBoolean(JsonValue value) {
        return value == null
                || (!ValueType.TRUE.equals(value.getValueType())
                        && !ValueType.FALSE.equals(value.getValueType()));
    }

    public static boolean isNotNumber(JsonValue value) {
        return value == null || !ValueType.NUMBER.equals(value.getValueType());
    }

    public static boolean isTrue(JsonValue value) {
        return value != null && ValueType.TRUE.equals(value.getValueType());
    }

    public static boolean isFalse(JsonValue value) {
        return value != null && ValueType.FALSE.equals(value.getValueType());
    }

    public static boolean isEmptyObject(JsonValue value) {
        return isObject(value) && value.asJsonObject().isEmpty();
    }

    public static boolean isEmptyArray(JsonValue value) {
        return isArray(value) && value.asJsonArray().isEmpty();
    }

    public static JsonObject toJsonObject(Map<String, JsonValue> map) {
        final JsonObjectBuilder builder = Json.createObjectBuilder();

        map.entrySet().forEach(e -> builder.add(e.getKey(), e.getValue()));

        return builder.build();
    }

    public static JsonObject merge(JsonObject target, JsonObject source) {
        Map<String, JsonValue> targetMap = (new LinkedHashMap<>(target));

        source.forEach(targetMap::put);

        return toJsonObject(targetMap);
    }

    public static Collection<JsonValue> toCollection(JsonValue value) {

        if (value == null) {
            return Collections.emptyList();
        }

        if (JsonValue.ValueType.ARRAY.equals(value.getValueType())) {
            return value.asJsonArray();
        }

        return Collections.singletonList(value);
    }

    public static Stream<JsonValue> toStream(JsonValue value) {

        if (value == null) {
            return Stream.empty();
        }

        if (JsonValue.ValueType.ARRAY.equals(value.getValueType())) {
            return value.asJsonArray().stream();
        }

        return Stream.of(value);
    }

    public static JsonArray toJsonArray(JsonValue value) {
        return JsonUtils.isArray(value)
                ? value.asJsonArray()
                : Json.createArrayBuilder().add(value).build();
    }

    public static boolean isBlankString(JsonValue value) {
        return isString(value) && isBlank(((JsonString) value).getString());
    }

    public static JsonValue toJsonValue(String value) {
        return value != null && isNotBlank(value)
                ? Json.createValue(value)
                : JsonValue.NULL;
    }

    public static boolean isNonEmptyArray(JsonValue value) {
        return isArray(value) && !value.asJsonArray().isEmpty();
    }

    public static boolean isNonEmptyObject(JsonValue value) {
        return isObject(value) && !value.asJsonObject().isEmpty();
    }

    public static JsonValue flatten(JsonValue value, String key) {

        if (JsonUtils.isArray(value) && value.asJsonArray().size() == 1) {
            value = value.asJsonArray().get(0);
        }

        if (JsonUtils.isObject(value) && value.asJsonObject().containsKey(key)) {
            value = value.asJsonObject().get(key);
        }

        return value;
    }

    static final boolean isBlank(final String string) {
        return string == null || string.isBlank();
    }

    static final boolean isNotBlank(final String string) {
        return string != null && !string.isBlank();
    }

}