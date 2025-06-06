package com.apicatalog.linkedtree.json;

import com.apicatalog.linkedtree.literal.IntegerValue;
import com.apicatalog.linkedtree.literal.NumericValue;
import com.apicatalog.linkedtree.xsd.XsdVocab;

import jakarta.json.JsonValue;

public class JsonInteger implements NumericValue, IntegerValue, JsonNode {

    protected jakarta.json.JsonNumber jsonValue;
    protected String datatype;

    protected JsonInteger() {
        // protected
    }

    public static JsonInteger of(jakarta.json.JsonNumber jsonNumber) {
        return of(jsonNumber, XsdVocab.INTEGER);
    }

    public static JsonInteger of(jakarta.json.JsonNumber jsonNumber, String datatype) {
        final JsonInteger number = new JsonInteger();
        number.jsonValue = jsonNumber;
        number.datatype = datatype;
        return number;
    }

    @Override
    public String lexicalValue() {
        return jsonValue.bigIntegerValue().toString();
    }

    @Override
    public Number numberValue() {
        return jsonValue.numberValue();
    }

    @Override
    public int integerValue() {
        return jsonValue.intValueExact();
    }

    @Override
    public String datatype() {
        return datatype;
    }

    @Override
    public String toString() {
        return "JsonInteger [json=" + jsonValue + ", datatype=" + datatype + "]";
    }
    
    @Override
    public JsonValue jsonValue() {
        return jsonValue;
    }
}
