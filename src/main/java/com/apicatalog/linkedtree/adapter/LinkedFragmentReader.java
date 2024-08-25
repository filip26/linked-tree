package com.apicatalog.linkedtree.adapter;

import java.util.Collection;
import java.util.Map;

import com.apicatalog.linkedtree.LinkedContainer;
import com.apicatalog.linkedtree.LinkedFragment;
import com.apicatalog.linkedtree.link.Link;
import com.apicatalog.linkedtree.pi.ProcessingInstruction;

@FunctionalInterface
public interface LinkedFragmentReader {

    LinkedFragment read(
            Link id,
            Collection<String> types,
            Map<String, LinkedContainer> properties,
            ProcessingInstruction pi);

}
