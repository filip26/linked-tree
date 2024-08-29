package com.apicatalog.linkedtree.primitive;

import java.util.Collection;
import java.util.Map;

import com.apicatalog.linkedtree.Linkable;
import com.apicatalog.linkedtree.LinkedContainer;
import com.apicatalog.linkedtree.LinkedFragment;
import com.apicatalog.linkedtree.LinkedNode;
import com.apicatalog.linkedtree.adapter.LinkedFragmentReader;
import com.apicatalog.linkedtree.link.Link;

/**
 * Allows to wrap a custom instance that does not inherit {@link LinkedNode} but
 * {@link Linkable}. Intended to be used as
 * {@link LinkedFragmentReader#read(Link, Collection, Map)} result.
 */
public class LinkableObject implements LinkedFragment {

    protected Link link;
    protected Collection<String> type;
    protected Map<String, LinkedContainer> entries;
    protected Linkable linkable;

    public LinkableObject(Link id, Collection<String> type, Map<String, LinkedContainer> entries) {
        this.link = id;
        this.type = type;
        this.entries = entries;
        this.linkable = null;
    }

    @Override
    public Collection<String> terms() {
        return entries.keySet();
    }

    @Override
    public LinkedContainer property(String term) {
        return entries.get(term);
    }

    public LinkableObject linkable(Linkable linkable) {
        this.linkable = linkable;
        return this;
    }

    @SuppressWarnings("unchecked")
    @Override
    public <T> T cast(Class<T> clazz) {
        return (T) linkable;
    }

    @Override
    public Link link() {
        return link;
    }

    @Override
    public Collection<String> type() {
        return type;
    }
}
