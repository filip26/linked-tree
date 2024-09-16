package com.apicatalog.linkedtree.json;

import java.util.Objects;

import com.apicatalog.linkedtree.LinkedLiteral;
import com.apicatalog.linkedtree.LinkedTree;

import jakarta.json.JsonString;
import jakarta.json.JsonValue;
import jakarta.json.JsonValue.ValueType;

public record JsonScalar(
        JsonValue jsonValue,
        String datatype,
        LinkedTree root) implements LinkedLiteral, JsonNode {

    @Override
    public String lexicalValue() {
        return ValueType.STRING == jsonValue.getValueType()
                ? ((JsonString) jsonValue).getString()
                : jsonValue.toString();
    }

    @Override
    public String toString() {
        return "JsonScalar [jsonValue=" + jsonValue + ", datatype=" + datatype + "]";
    }

    @Override
    public int hashCode() {
        return Objects.hash(datatype, jsonValue);
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj == null)
            return false;
        if (getClass() != obj.getClass())
            return false;
        JsonScalar other = (JsonScalar) obj;
        return Objects.equals(datatype, other.datatype) && Objects.equals(jsonValue, other.jsonValue);
    }
}
