package com.apicatalog.anchored;

import java.util.Collection;

public interface Graph extends Resource {

    @Override
    default NodeType nodeType() {
        return NodeType.Graph;
    }

    Collection<Node> items();
}
