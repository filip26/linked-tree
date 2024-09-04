package com.apicatalog.linkedtree.reader;

import java.util.Collection;
import java.util.Map;

import com.apicatalog.linkedtree.Link;
import com.apicatalog.linkedtree.LinkedContainer;
import com.apicatalog.linkedtree.LinkedFragment;
import com.apicatalog.linkedtree.builder.TreeBuilderContext;

@FunctionalInterface
public interface LinkedFragmentReader {

    LinkedFragment read(
            Link id,
            Collection<String> types,
            Map<String, LinkedContainer> properties,
            TreeBuilderContext ctx
            ) throws LinkedReaderError;

}
