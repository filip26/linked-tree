package com.apicatalog.linkedtree.builder;

import java.util.function.Supplier;

import com.apicatalog.linkedtree.LinkedTree;

public interface TreeBuilderContext {

    Supplier<LinkedTree> rootSupplier();

}
