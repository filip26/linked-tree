package com.apicatalog.linkedtree.json;

import com.apicatalog.linkedtree.LinkedLiteral;

import jakarta.json.JsonValue;

public record JsonScalar(
        JsonValue jsonValue,
        String datatype) implements LinkedLiteral {

    @Override
    public String lexicalValue() {
        return jsonValue.toString();
    }

}
