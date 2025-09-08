package com.apicatalog.anchored;

import java.util.Collection;

public interface Sequence extends Node {

    @Override
    default NodeType nodeType() {
        return NodeType.Sequence;
    }
    
    Collection<Node> items();
}
