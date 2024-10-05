package com.apicatalog.linkedtree.orm;

@Fragment
@Term("AlumniCredential")
@Vocab("https://www.w3.org/ns/credentials/examples#")
public interface ExtendedAnnotatedCredential extends AnnotatedCredential {

    @Term(value =  "credentialSubject", vocab = "https://www.w3.org/2018/credentials#")
    @Override
    AlumniSubject subject();
    
}
