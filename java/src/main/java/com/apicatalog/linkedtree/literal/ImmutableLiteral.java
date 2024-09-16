package com.apicatalog.linkedtree.literal;

import java.util.Objects;

import com.apicatalog.linkedtree.LinkedLiteral;
import com.apicatalog.linkedtree.LinkedTree;

public record ImmutableLiteral(
        String lexicalValue,
        String datatype,
        LinkedTree root) implements LinkedLiteral {

    @Override
    public int hashCode() {
        return Objects.hash(datatype, lexicalValue);
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj == null)
            return false;
        if (getClass() != obj.getClass())
            return false;
        ImmutableLiteral other = (ImmutableLiteral) obj;
        return Objects.equals(datatype, other.datatype) && Objects.equals(lexicalValue, other.lexicalValue);
    }

    @Override
    public String toString() {
        return "ImmutableLiteral [lexicalValue=" + lexicalValue + ", datatype=" + datatype + "]";
    }
}
