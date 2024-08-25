package com.apicatalog.linkedtree.json;

import com.apicatalog.linkedtree.literal.NumericValue;
import com.apicatalog.linkedtree.pi.ProcessingInstruction;
import com.apicatalog.linkedtree.xsd.XsdConstants;
import com.apicatalog.linkedtree.literal.IntegerValue;

public class JsonInteger implements NumericValue, IntegerValue {

    protected jakarta.json.JsonNumber json;
    protected String datatype;
    protected ProcessingInstruction pi;

    protected JsonInteger() {
        // protected
    }

    public static JsonInteger of(jakarta.json.JsonNumber jsonNumber, ProcessingInstruction pi) {
        return of(jsonNumber, XsdConstants.INTEGER, pi);
    }

    public static JsonInteger of(jakarta.json.JsonNumber jsonNumber, String datatype, ProcessingInstruction pi) {
        final JsonInteger number = new JsonInteger();
        number.json = jsonNumber;
        number.datatype = datatype;
        number.pi = pi;
        return number;
    }

    @Override
    public String lexicalValue() {
        return json.bigIntegerValue().toString();
    }

    @Override
    public Number numberValue() {
        return json.numberValue();
    }

    @Override
    public int integerValue() {
        return json.intValueExact();
    }

    @Override
    public String datatype() {
        return datatype;
    }

    @Override
    public ProcessingInstruction pi() {
        return pi;
    }

}
