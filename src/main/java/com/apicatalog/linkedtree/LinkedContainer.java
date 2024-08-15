package com.apicatalog.linkedtree;

import java.util.Collection;

public non-sealed interface LinkedContainer extends LinkedData {

    Collection<LinkedData> nodes();
    
    String containerType();
}
