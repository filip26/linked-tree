package com.apicatalog.linkedtree.fragment;

import java.util.Collection;
import java.util.Collections;

import com.apicatalog.linkedtree.literal.adapter.LiteralAdapter;

@FunctionalInterface
public interface LinkedFragmentAdapter {

    LinkedFragmentReader reader();

    default Collection<LiteralAdapter> literalAdapters() {
        return Collections.emptyList();
    }
}
