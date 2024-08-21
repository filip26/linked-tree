package com.apicatalog.linkedtree.primitive;

import java.util.Collection;

import com.apicatalog.linkedtree.Link;
import com.apicatalog.linkedtree.LinkedFragment;

public class GenericLink implements Link {

    protected String uri;
    
    protected GenericLink() {
    }
    
    public static GenericLink of(String uri) {
        final GenericLink link = new GenericLink();
        link.uri = uri;
        return link;
    }
    
    @Override
    public String uri() {
        return uri;
    }

    @Override
    public Collection<LinkedFragment> fragments() {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public LinkedFragment target() {
        // TODO Auto-generated method stub
        return null;
    }

    public void add(LinkedFragment node) {
        // TODO Auto-generated method stub
    }
    
}
