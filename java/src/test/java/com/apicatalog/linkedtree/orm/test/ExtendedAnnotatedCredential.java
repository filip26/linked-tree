package com.apicatalog.linkedtree.orm.test;

import com.apicatalog.linkedtree.orm.Context;
import com.apicatalog.linkedtree.orm.Fragment;
import com.apicatalog.linkedtree.orm.Term;
import com.apicatalog.linkedtree.orm.Vocab;

@Fragment
@Term("AlumniCredential")
@Vocab("https://www.w3.org/ns/credentials/examples#")
@Context("https://www.w3.org/ns/credentials/examples/v2")
public interface ExtendedAnnotatedCredential extends AnnotatedCredential {

    @Term(value = "credentialSubject", vocab = "https://www.w3.org/2018/credentials#")
    @Override
    AlumniSubject subject();

}
