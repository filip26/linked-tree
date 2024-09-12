package com.apicatalog.linkedtree.literal.adapter;

import com.apicatalog.linkedtree.LinkedLiteral;
import com.apicatalog.linkedtree.adapter.NodeAdapter;

public interface LiteralAdapter extends NodeAdapter<String, LinkedLiteral> {

    String datatype();
}
