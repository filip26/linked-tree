package com.apicatalog.linkedtree.fragment;

import java.util.Collection;
import java.util.Map;

import com.apicatalog.linkedtree.Linkable;
import com.apicatalog.linkedtree.LinkedContainer;
import com.apicatalog.linkedtree.LinkedFragment;
import com.apicatalog.linkedtree.LinkedNode;
import com.apicatalog.linkedtree.LinkedTree;
import com.apicatalog.linkedtree.link.Link;
import com.apicatalog.linkedtree.type.Type;

/**
 * Allows to wrap a custom instance that does not inherit {@link LinkedNode} but
 * {@link Linkable}. Intended to be used as
 * {@link LinkedFragmentReader#read(Link, Collection, Map)} result.
 */
@Deprecated
public record LinkableObject(
        Link id,
        Type type,
        Map<String, LinkedContainer> entries,
        LinkedTree root,
        Linkable linkable) implements LinkedFragment {

    @Override
    public Collection<String> terms() {
        return entries.keySet();
    }

    @Override
    public LinkedContainer container(String term) {
        return entries.get(term);
    }

//    @SuppressWarnings("unchecked")
//    @Override
//    public <T> T cast(Class<T> clazz) {
//        return (T) linkable;
//    }
//
//    @Override
//    public Linkable cast() {
//        return linkable;
//    }
}
