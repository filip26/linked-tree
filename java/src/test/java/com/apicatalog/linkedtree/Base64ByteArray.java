package com.apicatalog.linkedtree;

import java.util.Base64;

import com.apicatalog.linkedtree.literal.ByteArrayValue;
import com.apicatalog.linkedtree.literal.adapter.TypedLiteralAdapter;
import com.apicatalog.linkedtree.literal.adapter.GenericDatatypeAdapter;

public class Base64ByteArray implements ByteArrayValue {

    public static String TYPE = "https://test/base64array";

    String value;
    byte[] byteArray;
    LinkedTree root;

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

    public static TypedLiteralAdapter typeAdapter() {
        return new GenericDatatypeAdapter(TYPE, Base64ByteArray::of);
    }
    
    @Override
    public LinkedTree root() {
        return root;
    }
}
