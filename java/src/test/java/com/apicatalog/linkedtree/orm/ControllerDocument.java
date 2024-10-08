package com.apicatalog.linkedtree.orm;

import java.net.URI;
import java.util.Set;

import com.apicatalog.linkedtree.type.Type;

@Fragment(generic = true)
@Vocab("https://w3id.org/security#")
public interface ControllerDocument {

    @Id
    URI id();

    /**
     * An optional set of controller document types.
     * 
     * @return a selector of document types, never <code>null</code>.
     */
    Type type();
    
    Set<URI> controller();

    Set<VerificationMethod> verificationMethod();

    Set<URI> alsoKnownAs();

    @Term("authenticationMethod")
    Set<VerificationMethod> authentication();

    Set<VerificationMethod> assertionMethod();

    Set<VerificationMethod> keyAgreement();

    Set<VerificationMethod> capabilityInvocation();

    Set<VerificationMethod> capabilityDelegation();
}
