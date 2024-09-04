package com.apicatalog.linkedtree.primitive;

import java.util.Collection;
import java.util.Map;

import com.apicatalog.linkedtree.Linkable;
import com.apicatalog.linkedtree.LinkedContainer;
import com.apicatalog.linkedtree.LinkedFragment;
import com.apicatalog.linkedtree.LinkedNode;
import com.apicatalog.linkedtree.LinkedTree;
import com.apicatalog.linkedtree.link.Link;
import com.apicatalog.linkedtree.reader.LinkedFragmentReader;

/**
 * Allows to wrap a custom instance that does not inherit {@link LinkedNode} but
 * {@link Linkable}. Intended to be used as
 * {@link LinkedFragmentReader#read(Link, Collection, Map)} result.
 */
public record LinkableObject(
        Link id,
        Collection<String> type,
        Map<String, LinkedContainer> entries,
        LinkedTree root,
        Linkable linkable) implements LinkedFragment {

    @Override
    public Collection<String> terms() {
        return entries.keySet();
    }

    @Override
    public LinkedContainer property(String term) {
        return entries.get(term);
    }

    @SuppressWarnings("unchecked")
    @Override
    public <T> T cast(Class<T> clazz) {
        return (T) linkable;
    }

    @Override
    public Linkable cast() {
        return linkable;
    }

    @Override
    public Link id() {
        return id;
    }

    @Override
    public Collection<String> type() {
        return type;
    }
}
