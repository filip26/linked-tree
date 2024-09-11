package com.apicatalog.linkedtree.adapter;

import java.util.Collection;
import java.util.Collections;

import com.apicatalog.linkedtree.reader.LinkedFragmentReader;

@FunctionalInterface
public interface LinkedFragmentAdapter {

    LinkedFragmentReader reader();

    default Collection<LiteralAdapter> literalAdapters() {
        return Collections.emptyList();
    }
}
