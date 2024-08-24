package com.apicatalog.linkedtree.json;

import com.apicatalog.linkedtree.literal.NumericValue;
import com.apicatalog.linkedtree.xsd.XsdConstants;
import com.apicatalog.linkedtree.literal.IntegerValue;

public class JsonInteger implements NumericValue, IntegerValue {

    protected jakarta.json.JsonNumber json;
    protected String datatype;
    protected Object meta;
    
    protected JsonInteger() {
        // protected
    }
    
    public static JsonInteger of(jakarta.json.JsonNumber jsonNumber, Object meta) {
        return of(jsonNumber, XsdConstants.INTEGER, meta);
    }
    
    public static JsonInteger of(jakarta.json.JsonNumber jsonNumber, String datatype, Object meta) {
        final JsonInteger number = new JsonInteger();
        number.json = jsonNumber;
        number.datatype = datatype;
        number.meta = meta;
        return number;
    }
    
    @Override
    public String value() {
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
    public Object pi() {
        return meta;
    }

}
