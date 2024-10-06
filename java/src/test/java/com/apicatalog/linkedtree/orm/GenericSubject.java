package com.apicatalog.linkedtree.orm;

import java.net.URI;

import com.apicatalog.linkedtree.type.Type;

@Fragment(generic = true)
public interface GenericSubject {

    @Id
    URI id();

    Type type();
    
}
