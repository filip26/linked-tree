package com.apicatalog.ld.anchor;

import java.util.Collection;

public interface Sequence extends Node {

    @Override
    default NodeType nodeType() {
        return NodeType.Sequence;
    }
    
    Collection<Node> items();
}
