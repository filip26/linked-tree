package com.apicatalog.linkedtree.xsd;

import java.time.Instant;

import com.apicatalog.linkedtree.LinkedLiteral;

public class XsdDateTime implements LinkedLiteral {

    public static final String TYPE = XsdConstants.DATE_TIME;
    
    protected Instant datetime;
    protected String value;
    protected Object meta;

    protected XsdDateTime(String value, Object meta) {
        this.value = value;
        this.meta = meta;
        this.datetime = null;
    }

    public static XsdDateTime of(String value, Object meta) {
        return new XsdDateTime(value, meta);
    }

    public Instant datetime() {
        if (datetime == null && value != null) {
            //TODO check
            datetime = Instant.parse(value);
        }

        return datetime;
    }

    @Override
    public String value() {
        if (value == null && datetime != null) {
            //TODO
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
    
    @Override
    public Object pi() {
        return meta;
    }
}
