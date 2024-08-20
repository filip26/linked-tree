package com.apicatalog.linkedtree;

import java.util.Collection;

public non-sealed interface LinkedTree extends LinkedData {

    // root fragments
    Collection<LinkedFragment> fragments();

    // identifiable fragments
    Collection<Link> links();

    // TODO predicates. i.e. terms???
}
