package com.apicatalog.ld.anchor;

public interface Graph extends Resource, Sequence {

    @Override
    default NodeType nodeType() {
        return NodeType.Graph;
    }
}
