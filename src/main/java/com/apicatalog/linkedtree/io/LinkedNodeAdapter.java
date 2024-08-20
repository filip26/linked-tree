package com.apicatalog.linkedtree.io;

import java.net.URI;
import java.util.Collection;
import java.util.Map;

import com.apicatalog.linkedtree.Link;
import com.apicatalog.linkedtree.LinkedData;
import com.apicatalog.linkedtree.LinkedFragment;

public interface LinkedNodeAdapter<O extends LinkedFragment> {

    /* future version should provide LinkedNodeReaderResolver or something like that */ 
    boolean accepts(URI id, Collection<String> types);

    O read(Context ctx, Link id, Collection<String> types, Map<String, LinkedData> properties);
    
    public interface Context {

//        Collection<LinkedData> read(String key, I value);
        
    }
    
}
