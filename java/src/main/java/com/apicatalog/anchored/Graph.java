package com.apicatalog.anchored;

import java.util.Collection;

public interface Graph extends Structure {

    @Override
    default NodeType nodeType() {
        return NodeType.Graph;
    }

    Collection<Node> items();
}
