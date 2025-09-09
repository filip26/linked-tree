package com.apicatalog.anchored;

import java.util.Collection;

public interface Anchored {

    Node root();

    Collection<String> ids();

    Collection<Structure> resources(String id);
}
