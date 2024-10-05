package com.apicatalog.linkedtree.orm.adapter;

import com.apicatalog.linkedtree.LinkedLiteral;
import com.apicatalog.linkedtree.adapter.NodeAdapterError;
import com.apicatalog.linkedtree.literal.adapter.TypedLiteralAdapter;

public interface NativeLiteralAdapter extends TypedLiteralAdapter {

    Object materialize(Class<?> type, LinkedLiteral literal) throws NodeAdapterError;
    
}
