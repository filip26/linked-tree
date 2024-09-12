package com.apicatalog.linkedtree.adapter;

import com.apicatalog.linkedtree.LinkedNode;
import com.apicatalog.linkedtree.LinkedTree;

public interface NodeAdapter<T, R extends LinkedNode> {

    R materialize(T source, LinkedTree root) throws AdapterError;
    
}
