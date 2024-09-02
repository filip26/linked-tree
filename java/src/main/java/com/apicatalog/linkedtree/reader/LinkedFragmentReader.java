package com.apicatalog.linkedtree.reader;

import java.util.Collection;
import java.util.Map;
import java.util.function.Supplier;

import com.apicatalog.linkedtree.Link;
import com.apicatalog.linkedtree.LinkedContainer;
import com.apicatalog.linkedtree.LinkedFragment;
import com.apicatalog.linkedtree.LinkedTree;

@FunctionalInterface
public interface LinkedFragmentReader {

    LinkedFragment read(
            Link id,
            Collection<String> types,
            Map<String, LinkedContainer> properties,
            Supplier<LinkedTree> rootSupplier
            ) throws LinkedReaderError;

}