package com.apicatalog.linkedtree.orm.test;

import java.net.URI;

import com.apicatalog.linkedtree.orm.Fragment;
import com.apicatalog.linkedtree.orm.Id;
import com.apicatalog.linkedtree.orm.Vocab;
import com.apicatalog.linkedtree.test.Status;

@Fragment
@Vocab("https://www.w3.org/ns/credentials/status#")
public interface BitstringStatusListEntry extends Status {
    
    @Id
    URI id();
    
}
