package com.apicatalog.linkedtree;

import java.util.Collection;

public non-sealed interface LinkedContainer extends LinkedData {

    Collection<LinkedData> data();
    
    String containerType();
}
