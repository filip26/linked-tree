package com.apicatalog.linkedtree.json;

import java.util.Collection;

import com.apicatalog.linkedtree.Link;
import com.apicatalog.linkedtree.LinkedFragment;
import com.apicatalog.linkedtree.LinkedTree;

public class JsonLinkedTree implements LinkedTree {

    protected Collection<LinkedFragment> fragments;

    public JsonLinkedTree(Collection<LinkedFragment> fragments) {
        this.fragments = fragments;
    }

    @Override
    public Collection<LinkedFragment> fragments() {
        return fragments;
    }

    @Override
    public Collection<Link> links() {
        // TODO Auto-generated method stub
        return null;
    }
}
