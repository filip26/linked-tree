package com.apicatalog.linkedtree.orm;

@Fragment
@Term("AlumniCredential")
@Vocab("https://www.w3.org/ns/credentials/examples#")
public interface ExtendedAnnotatedCredential extends AnnotatedCredential {

    @Override
    AlumniSubject subject();
    
}
