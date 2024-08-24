package com.apicatalog.linkedtree.io;

import java.util.Collection;
import java.util.Map;

import com.apicatalog.linkedtree.LinkedContainer;
import com.apicatalog.linkedtree.LinkedFragment;
import com.apicatalog.linkedtree.link.Link;

public interface LinkedFragmentAdapter {

    /*
     * future version should provide LinkedNodeReaderResolver or something like that
     */
    boolean accepts(String id, Collection<String> types);

    LinkedFragment read(Link id, Collection<String> types, Map<String, LinkedContainer> properties, Object meta);

//    public interface Context {
//
////        Collection<LinkedData> read(String key, I value);
//
//    }

}
