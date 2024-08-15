package com.apicatalog.linkedtree;

import java.net.URI;
import java.util.Collection;

public non-sealed interface LinkedNode extends LinkedData {

    URI id();

    Collection<String> type();
    
    Collection<String> terms();

    Collection<LinkedData> data(String term);
}
