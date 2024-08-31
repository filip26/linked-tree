package com.apicatalog.linkedtree.adapter;

import java.util.Collection;
import java.util.Collections;

@FunctionalInterface
public interface LinkedFragmentAdapter {

    LinkedFragmentReader reader();

    default Collection<LinkedLiteralAdapter> literalAdapters() {
        return Collections.emptyList();
    }
}
