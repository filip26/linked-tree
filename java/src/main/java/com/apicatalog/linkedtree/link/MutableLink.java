package com.apicatalog.linkedtree.link;

import java.util.ArrayList;
import java.util.Collection;

import com.apicatalog.linkedtree.LinkedFragment;

public class MutableLink implements Link {

    protected final String uri;
    protected Collection<LinkedFragment> fragments;
    protected LinkedFragment target;

    protected MutableLink(String uri, Collection<LinkedFragment> fragments) {
        this.uri = uri;
        this.fragments = fragments;
        this.target = null;
    }

    public static MutableLink of(String uri) {
        return new MutableLink(uri, new ArrayList<>());
    }

    @Override
    public String uri() {
        return uri;
    }

    @Override
    public Collection<LinkedFragment> refs() {
        return fragments;
    }

    @Override
    public LinkedFragment target() {
        return target;
    }

    public void target(LinkedFragment fragment) {
        this.target = fragment;
    }

    public void addFragment(LinkedFragment node) {
        this.fragments.add(node);
    }
}
