package com.apicatalog.linkedtree.primitive;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Objects;

import com.apicatalog.linkedtree.Link;
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

    @Override
    public int hashCode() {
        return Objects.hash(uri);
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj == null)
            return false;
        if (getClass() != obj.getClass())
            return false;
        MutableLink other = (MutableLink) obj;
        return Objects.equals(uri, other.uri);
    }

    @Override
    public String toString() {
        return "MutableLink [uri=" + uri + "]";
    }
}
