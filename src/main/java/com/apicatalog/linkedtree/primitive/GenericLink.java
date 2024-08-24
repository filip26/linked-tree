package com.apicatalog.linkedtree.primitive;

import java.util.ArrayList;
import java.util.Collection;

import com.apicatalog.linkedtree.Link;
import com.apicatalog.linkedtree.LinkedFragment;

public class GenericLink implements Link {

    protected String uri;
    protected Collection<LinkedFragment> fragments;
    protected LinkedFragment target;

    protected GenericLink() {
        // protected
    }

    public static GenericLink of(String uri) {
        final GenericLink link = new GenericLink();
        link.uri = uri;
        link.fragments = new ArrayList<>();
        link.target = null;
        return link;
    }

    @Override
    public String uri() {
        return uri;
    }

    @Override
    public Collection<LinkedFragment> fragments() {
        return fragments;
    }

    @Override
    public LinkedFragment target() {
        return target;
    }

    public void target(LinkedFragment fragment) {
        this.target = fragment;
    }

    public void add(LinkedFragment node) {
        this.fragments.add(node);
    }
}
