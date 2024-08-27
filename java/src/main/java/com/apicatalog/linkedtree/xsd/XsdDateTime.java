package com.apicatalog.linkedtree.xsd;

import java.time.Instant;

import com.apicatalog.linkedtree.LinkedLiteral;
import com.apicatalog.linkedtree.literal.DateTimeValue;

public class XsdDateTime implements LinkedLiteral, DateTimeValue {

    public static final String TYPE = XsdConstants.DATE_TIME;

    protected Instant datetime;
    protected String value;

    protected XsdDateTime(String value) {
        this.value = value;
        this.datetime = null;
    }

    protected XsdDateTime(Instant datetime) {
        this.value = null;
        this.datetime = datetime;
    }

    public static XsdDateTime of(String value) {
        return new XsdDateTime(value);
    }

    public static XsdDateTime of(Instant datetime) {
        return new XsdDateTime(datetime);
    }

    @Override
    public Instant datetime() {
        if (datetime == null && value != null) {
            // TODO check
            datetime = Instant.parse(value);
        }

        return datetime;
    }

    @Override
    public String lexicalValue() {
        if (value == null && datetime != null) {
            // TODO
        }
        return value;
    }

    @Override
    public String datatype() {
        return TYPE;
    }

    public void value(String value) {
        this.value = value;
        this.datetime = null;
    }

    public void datetime(Instant datetime) {
        this.datetime = datetime;
        this.value = null;
    }
}
