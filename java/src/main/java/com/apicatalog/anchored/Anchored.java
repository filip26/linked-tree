package com.apicatalog.anchored;

import java.util.Collection;

public interface Anchored {

    Node root();

    Collection<String> ids();

    Collection<Resource> resources(String id);
}
