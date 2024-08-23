package com.apicatalog.linkedtree.json;

import com.apicatalog.linkedtree.LinkedLiteral;
import com.apicatalog.linkedtree.rdf.RdfConstants;

import jakarta.json.JsonValue;

public class JsonLiteral implements LinkedLiteral {

    protected JsonValue json;
    protected String value;

    protected JsonLiteral() {
        // protected
    }

    public static JsonLiteral of(JsonValue value, Object meta) {
        final JsonLiteral literal = new JsonLiteral();
        literal.json = value;
        literal.value = null;
        return literal;
    }

    @Override
    public String value() {
        if (value == null) {
            value = JsonCanonicalizer.canonicalize(json);
        }
        return value;
    }

    public JsonValue jsonValue() {
        return json;
    }

    @Override
    public String datatype() {
        return RdfConstants.JSON;
    }
}
