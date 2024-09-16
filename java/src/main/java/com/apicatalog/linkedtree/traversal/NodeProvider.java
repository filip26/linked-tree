package com.apicatalog.linkedtree.traversal;

import com.apicatalog.linkedtree.LinkedNode;

public interface NodeProvider {

    void begin(LinkedNode node);
    void end(LinkedNode node);
}
