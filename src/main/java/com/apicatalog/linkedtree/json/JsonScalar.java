package com.apicatalog.linkedtree.json;

import com.apicatalog.linkedtree.LinkedLiteral;

import jakarta.json.JsonValue;

public class JsonScalar implements LinkedLiteral {

    protected JsonValue json;
    protected String datatype;
    protected Object meta;

    protected JsonScalar() {
        // protected
    }

    public static JsonScalar of(JsonValue json, String datatype, Object meta) {
        final JsonScalar scalar = new JsonScalar();
        scalar.json = json;
        scalar.datatype = datatype;
        scalar.meta = meta;
        return scalar;
    }

    @Override
    public String value() {
        return json.toString();
    }

    @Override
    public String datatype() {
        return datatype;
    }

    public JsonValue jsonValue() {
        return json;
    }

    @Override
    public Object metadata() {
        return meta;
    }

}
