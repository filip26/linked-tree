package com.apicatalog.linkedtree.json;

import java.text.DecimalFormat;
import java.text.DecimalFormatSymbols;
import java.util.Locale;

import com.apicatalog.linkedtree.LinkedTree;
import com.apicatalog.linkedtree.literal.DoubleValue;
import com.apicatalog.linkedtree.literal.NumericValue;
import com.apicatalog.linkedtree.xsd.XsdVocab;

public class JsonDecimal implements NumericValue, DoubleValue, JsonNode {

    protected static final DecimalFormat xsdNumberFormat = new DecimalFormat("0.0##############E0", new DecimalFormatSymbols(Locale.ENGLISH));

    static {
        xsdNumberFormat.setMinimumFractionDigits(1);
    }

    protected jakarta.json.JsonNumber jsonValue;
    protected String datatype;
    protected LinkedTree root;

    protected JsonDecimal() {
        // protected
    }

    public static JsonDecimal of(jakarta.json.JsonNumber jsonNumber, LinkedTree root) {
        return of(jsonNumber, XsdVocab.DOUBLE, root);
    }

    public static JsonDecimal of(jakarta.json.JsonNumber jsonNumber, String datatype, LinkedTree root) {
        final JsonDecimal number = new JsonDecimal();
        number.jsonValue = jsonNumber;
        number.datatype = datatype;
        number.root = root;
        return number;
    }

    @Override
    public String lexicalValue() {
        return xsdNumberFormat.format(jsonValue.bigDecimalValue());
    }

    @Override
    public Number numberValue() {
        return jsonValue.numberValue();
    }

    @Override
    public String datatype() {
        return datatype;
    }

    @Override
    public double doubleValue() {
        return jsonValue.doubleValue();
    }

    @Override
    public LinkedTree root() {
        return root;
    }

    @Override
    public String toString() {
        return "JsonDecimal [json=" + jsonValue + ", datatype=" + datatype + "]";
    }
    
    @Override
    public jakarta.json.JsonNumber jsonValue() {
        return jsonValue;
    }
}
