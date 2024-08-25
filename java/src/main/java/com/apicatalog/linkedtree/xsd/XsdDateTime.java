package com.apicatalog.linkedtree.xsd;

import java.time.Instant;

import com.apicatalog.linkedtree.LinkedLiteral;
import com.apicatalog.linkedtree.literal.DateTimeValue;
import com.apicatalog.linkedtree.pi.ProcessingInstruction;

public class XsdDateTime implements LinkedLiteral, DateTimeValue {

    public static final String TYPE = XsdConstants.DATE_TIME;

    protected Instant datetime;
    protected String value;
    protected ProcessingInstruction pi;

    protected XsdDateTime(String value, ProcessingInstruction pi) {
        this.value = value;
        this.pi = pi;
        this.datetime = null;
    }

    protected XsdDateTime(Instant datetime, ProcessingInstruction pi) {
        this.value = null;
        this.pi = pi;
        this.datetime = datetime;
    }

    public static XsdDateTime of(String value, ProcessingInstruction pi) {
        return new XsdDateTime(value, pi);
    }

    public static XsdDateTime of(Instant datetime, ProcessingInstruction pi) {
        return new XsdDateTime(datetime, pi);
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

    @Override
    public ProcessingInstruction pi() {
        return pi;
    }
}
