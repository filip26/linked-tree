package com.apicatalog.linkedtree.orm.getter;

import com.apicatalog.linkedtree.LinkedFragment;
import com.apicatalog.linkedtree.adapter.NodeAdapterError;

public interface Getter {

    Object materialize(LinkedFragment source) throws NodeAdapterError;

}
