package com.apicatalog.linkedtree.json;

import com.apicatalog.linkedtree.LinkedLiteral;
import com.apicatalog.linkedtree.pi.ProcessingInstruction;

import jakarta.json.JsonValue;

public record JsonScalar(
        JsonValue jsonValue,
        String datatype,
        ProcessingInstruction pi
        ) implements LinkedLiteral {

    @Override
    public String value() {
        return jsonValue.toString();
    }

}
