package com.apicatalog.anchored;

public interface Node {

    public enum NodeType {
        Graph,
        Fragment,
        Literal,
        Sequence,
    }

    NodeType nodeType();
}
