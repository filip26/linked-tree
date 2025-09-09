package com.apicatalog.anchored;

import java.util.Collection;
import java.util.Set;

public interface Anchored {

    Resource root();

    Set<String> ids();

    Collection<Resource> resources(String id);
}
