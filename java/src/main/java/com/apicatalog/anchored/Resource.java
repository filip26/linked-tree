package com.apicatalog.anchored;

import java.util.Collection;

public interface Resource extends Node {

    Collection<String> predicates();

    Node object(String id);
}
