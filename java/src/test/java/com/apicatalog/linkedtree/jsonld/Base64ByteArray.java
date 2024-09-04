package com.apicatalog.linkedtree.jsonld;

import java.util.Base64;

import com.apicatalog.linkedtree.LinkedTree;
import com.apicatalog.linkedtree.literal.ByteArrayValue;

class Base64ByteArray implements ByteArrayValue {

    static String TYPE = "https://test/base64array";

    String value;
    byte[] byteArray;
    
    final LinkedTree root;

    Base64ByteArray(String value, LinkedTree root) {
        this.value = value;
        this.root = root;
    }

    public static Base64ByteArray of(String value, LinkedTree root) {
        return new Base64ByteArray(value, root);
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
    public LinkedTree root() {
        return root;
    }
}
