package com.apicatalog.linkedtree.json;

import com.apicatalog.linkedtree.LinkedLiteral;
import com.apicatalog.linkedtree.pi.ProcessingInstruction;
import com.apicatalog.linkedtree.rdf.RdfConstants;

import jakarta.json.JsonValue;

public class JsonLiteral implements LinkedLiteral {

    protected JsonValue json;
    protected String value;
    protected ProcessingInstruction pi;

    protected JsonLiteral() {
        // protected
    }

    public static JsonLiteral of(JsonValue value, ProcessingInstruction pi) {
        final JsonLiteral literal = new JsonLiteral();
        literal.json = value;
        literal.value = null;
        literal.pi = pi;
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

    @Override
    public ProcessingInstruction pi() {
        return pi;
    }
}
