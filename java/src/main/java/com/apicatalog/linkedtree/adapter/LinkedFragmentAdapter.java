package com.apicatalog.linkedtree.adapter;

import java.util.Collection;

public interface LinkedFragmentAdapter extends LinkedFragmentReader {

    /*
     * future version should provide LinkedNodeReaderResolver or something like that
     */
    boolean accepts(String id, Collection<String> types);

}
