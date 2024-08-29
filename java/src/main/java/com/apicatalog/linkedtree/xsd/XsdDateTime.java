package com.apicatalog.linkedtree.xsd;

import java.time.Instant;
import java.time.OffsetDateTime;
import java.time.format.DateTimeParseException;

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

//            datetime = Instant.parse(value);
            try {
                OffsetDateTime createdOffset = OffsetDateTime.parse(value);

                datetime = createdOffset.toInstant();

            } catch (DateTimeParseException e) {
                throw new IllegalArgumentException(e);
            }

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
