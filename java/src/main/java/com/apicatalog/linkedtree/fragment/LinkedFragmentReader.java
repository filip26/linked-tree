package com.apicatalog.linkedtree.fragment;

import java.util.Map;

import com.apicatalog.linkedtree.LinkedContainer;
import com.apicatalog.linkedtree.LinkedFragment;
import com.apicatalog.linkedtree.builder.TreeBuilderError;
import com.apicatalog.linkedtree.link.Link;
import com.apicatalog.linkedtree.type.Type;

@FunctionalInterface
public interface LinkedFragmentReader {

    LinkedFragment read(
            Link id,
            Type type,
            Map<String, LinkedContainer> properties) throws TreeBuilderError;

}
