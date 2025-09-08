package com.apicatalog.anchored;

import java.util.Collection;

public interface Fragment extends Resource {

    @Override
    default NodeType nodeType() {
        return NodeType.Fragment;
    }
    
    Graph graph();
    
    Collection<String> type();
}
