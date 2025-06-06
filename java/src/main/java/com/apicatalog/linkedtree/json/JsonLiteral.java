package com.apicatalog.linkedtree.json;

import com.apicatalog.linkedtree.LinkedLiteral;
import com.apicatalog.linkedtree.rdf.RdfVocab;

import jakarta.json.JsonValue;

public class JsonLiteral implements LinkedLiteral, JsonNode {

    protected JsonValue jsonValue;
    protected String value;

    protected JsonLiteral() {
        // protected
    }

    public static JsonLiteral of(JsonValue value) {
        final JsonLiteral literal = new JsonLiteral();
        literal.jsonValue = value;
        literal.value = null;
        return literal;
    }

    // TODO public static JsonLiteral of(String value)

    @Override
    public String lexicalValue() {
        if (value == null) {
            value = JsonCanonicalizer.canonicalize(jsonValue);
        }
        return value;
    }

    @Override
    public JsonValue jsonValue() {
        return jsonValue;
    }

    @Override
    public String datatype() {
        return RdfVocab.JSON;
    }

    @Override
    public String toString() {
        return "JsonLiteral [value=" + value + "]";
    }

}
