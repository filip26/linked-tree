package com.apicatalog.linkedtree.orm.test;

import java.net.URI;

import com.apicatalog.linkedtree.orm.Fragment;
import com.apicatalog.linkedtree.orm.Id;
import com.apicatalog.linkedtree.type.FragmentType;

@Fragment(generic = true)
public interface GenericSubject {

    @Id
    URI id();

    FragmentType type();
    
}
