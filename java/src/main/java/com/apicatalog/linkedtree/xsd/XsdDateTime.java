package com.apicatalog.linkedtree.xsd;

import java.time.Instant;
import java.time.OffsetDateTime;
import java.time.format.DateTimeParseException;

import com.apicatalog.linkedtree.LinkedLiteral;
import com.apicatalog.linkedtree.LinkedTree;
import com.apicatalog.linkedtree.literal.DateTimeValue;
import com.apicatalog.linkedtree.literal.adapter.DatatypeAdapter;
import com.apicatalog.linkedtree.literal.adapter.GenericDatatypeAdapter;

public class XsdDateTime implements LinkedLiteral, DateTimeValue {

    public static final String TYPE = XsdConstants.DATE_TIME;

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

    protected static DatatypeAdapter ADAPTER = new GenericDatatypeAdapter(
            TYPE,
            XsdDateTime::of);

    public static DatatypeAdapter typeAdapter() {
        return ADAPTER;
    }

    @Override
    public String toString() {
        return "XsdDateTime [datetime=" + datetime + ", value=" + value + "]";
    }

    @Override
    public LinkedTree root() {
        return root;
    }
}
