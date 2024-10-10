package com.apicatalog.linkedtree.xsd;

import java.time.Instant;
import java.time.OffsetDateTime;
import java.time.format.DateTimeParseException;

import com.apicatalog.linkedtree.LinkedLiteral;
import com.apicatalog.linkedtree.LinkedTree;
import com.apicatalog.linkedtree.literal.DateTimeValue;

public class XsdDateTime implements LinkedLiteral, DateTimeValue {

    protected Instant datetime;
    protected String value;
    protected LinkedTree root;

    protected XsdDateTime(String value, LinkedTree root) {
        this.value = value;
        this.datetime = null;
        this.root = root;
    }

    protected XsdDateTime(Instant datetime, LinkedTree root) {
        this.value = null;
        this.datetime = datetime;
        this.root = root;
    }

    public static XsdDateTime of(String value, LinkedTree root) {
        return new XsdDateTime(value, root);
    }

    public static XsdDateTime of(Instant datetime, LinkedTree root) {
        return new XsdDateTime(datetime, root);
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

    @Override
    public LinkedTree root() {
        return root;
    }
    
    public static XsdDateTimeAdapter typeAdapter() {
        return XsdDateTimeAdapter.INSTANCE;
    }
}
