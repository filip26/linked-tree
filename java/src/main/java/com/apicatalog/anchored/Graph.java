package com.apicatalog.anchored;

public interface Graph extends Resource, Sequence {

    @Override
    default NodeType nodeType() {
        return NodeType.Graph;
    }
}
