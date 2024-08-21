package com.apicatalog.linkedtree.json;

import java.net.URI;
import java.util.Collection;

import com.apicatalog.linkedtree.Link;
import com.apicatalog.linkedtree.LinkedFragment;

import jakarta.json.JsonObject;

public class GenericLink implements Link {

    protected URI uri;
    
    
    public GenericLink(URI uri) {
        this.uri = uri;
    }
    
    @Override
    public URI uri() {
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
