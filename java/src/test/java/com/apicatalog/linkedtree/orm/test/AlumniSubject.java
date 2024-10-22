package com.apicatalog.linkedtree.orm.test;

import com.apicatalog.linkedtree.orm.Fragment;
import com.apicatalog.linkedtree.orm.Vocab;

@Fragment
@Vocab("https://www.w3.org/ns/credentials/examples#")
public interface AlumniSubject extends GenericSubject {

    String alumniOf();
}
