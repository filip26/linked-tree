package com.apicatalog.linkedtree.reader;

import java.util.Collection;
import java.util.Map;

import com.apicatalog.linkedtree.LinkedContainer;
import com.apicatalog.linkedtree.LinkedFragment;
import com.apicatalog.linkedtree.LinkedTreeError;
import com.apicatalog.linkedtree.link.Link;

@FunctionalInterface
public interface LinkedFragmentReader {

    LinkedFragment read(
            Link id,
            Collection<String> types,
            Map<String, LinkedContainer> properties) throws LinkedTreeError;

}
