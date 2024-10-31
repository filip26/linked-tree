package com.apicatalog.linkedtree.xsd;

import java.time.Instant;
import java.time.OffsetDateTime;
import java.time.format.DateTimeParseException;

import com.apicatalog.linkedtree.LinkedLiteral;
import com.apicatalog.linkedtree.literal.DateTimeValue;

public class XsdDateTime implements LinkedLiteral, DateTimeValue {

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
    public Instant datetime() throws DateTimeParseException {
        if (datetime == null && value != null) {
            OffsetDateTime createdOffset = OffsetDateTime.parse(value);
            datetime = createdOffset.toInstant();
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
        return XsdVocab.DATE_TIME;
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
    public String toString() {
        return "XsdDateTime [datetime=" + datetime + ", value=" + value + "]";
    }

    public static XsdDateTimeAdapter typeAdapter() {
        return XsdDateTimeAdapter.INSTANCE;
    }
}
