package com.apicatalog.linkedtree.jsonld;

import java.util.Base64;

import com.apicatalog.linkedtree.literal.ByteArrayValue;
import com.apicatalog.linkedtree.pi.ProcessingInstruction;

class Base64ByteArray implements ByteArrayValue {

    static String TYPE = "https://test/base64array";

    String value;
    byte[] byteArray;

    ProcessingInstruction pi;

    Base64ByteArray(String value, ProcessingInstruction pi) {
        this.value = value;
        this.pi = pi;
    }

    public static Base64ByteArray of(String value, ProcessingInstruction pi) {
        return new Base64ByteArray(value, pi);
    }

    @Override
    public String lexicalValue() {
        if (value == null && byteArray != null) {
            value = Base64.getEncoder().encodeToString(byteArray);
        }
        return value;
    }

    @Override
    public String datatype() {
        return TYPE;
    }

    @Override
    public byte[] byteArrayValue() {
        if (byteArray == null && value != null) {
            byteArray = Base64.getDecoder().decode(value);
        }

        return byteArray;
    }

    public void value(String value) {
        this.value = value;
        this.byteArray = null;
    }

    public void byteArrayValue(byte[] byteArray) {
        this.byteArray = byteArray;
        this.value = null;
    }

    @Override
    public ProcessingInstruction pi() {
        return pi;
    }
}
