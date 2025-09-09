package com.apicatalog.ld.anchor;

public interface Node {

    public enum NodeType {
        Graph,
        Fragment,
        Sequence,
        Literal,
    }

    NodeType nodeType();
}
