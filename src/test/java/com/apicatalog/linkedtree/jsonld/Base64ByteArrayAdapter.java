package com.apicatalog.linkedtree.jsonld;

import com.apicatalog.linkedtree.LinkedLiteral;
import com.apicatalog.linkedtree.io.LinkedLiteralAdapter;

public class Base64ByteArrayAdapter implements LinkedLiteralAdapter {

    @Override
    public String datatype() {
        return Base64ByteArray.TYPE;
    }

    @Override
    public LinkedLiteral read(String value, Object meta) {
        final Base64ByteArray array = new Base64ByteArray();
        array.value = value;
        array.meta = meta;
        return array;
    }

}
