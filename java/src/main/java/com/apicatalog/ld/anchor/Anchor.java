package com.apicatalog.ld.anchor;

import java.util.Collection;
import java.util.Set;

public interface Anchor {

    Resource root();

    Set<String> ids();

    Collection<Resource> resources(String id);
}
