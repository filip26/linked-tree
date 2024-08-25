package com.apicatalog.linkedtree.json;

import java.text.DecimalFormat;
import java.text.DecimalFormatSymbols;
import java.util.Locale;

import com.apicatalog.linkedtree.literal.DoubleValue;
import com.apicatalog.linkedtree.literal.NumericValue;
import com.apicatalog.linkedtree.pi.ProcessingInstruction;
import com.apicatalog.linkedtree.xsd.XsdConstants;

public class JsonDecimal implements NumericValue, DoubleValue {

    protected static final DecimalFormat xsdNumberFormat = new DecimalFormat("0.0##############E0", new DecimalFormatSymbols(Locale.ENGLISH));

    static {
        xsdNumberFormat.setMinimumFractionDigits(1);
    }
    
    protected jakarta.json.JsonNumber json;
    protected String datatype;
    protected ProcessingInstruction pi;
    
    protected JsonDecimal() {
        // protected
    }
    
    public static JsonDecimal of(jakarta.json.JsonNumber jsonNumber, ProcessingInstruction pi) {
        return of(jsonNumber, XsdConstants.DOUBLE, pi);
    }
    
    public static JsonDecimal of(jakarta.json.JsonNumber jsonNumber, String datatype, ProcessingInstruction pi) {
        final JsonDecimal number = new JsonDecimal();
        number.json = jsonNumber;
        number.datatype = datatype;
        number.pi = pi;
        return number;
    }
    
    @Override
    public String lexicalValue() {
        return xsdNumberFormat.format(json.bigDecimalValue());
    }

    @Override
    public Number numberValue() {
        return json.numberValue();
    }

    @Override
    public String datatype() {
        return datatype;
    }

    @Override
    public double doubleValue() {
        return json.doubleValue();
    }

    @Override
    public ProcessingInstruction pi() {
        return pi;
    }

}