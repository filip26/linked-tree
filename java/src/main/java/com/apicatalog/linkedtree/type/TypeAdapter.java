package com.apicatalog.linkedtree.type;

import java.util.Collection;
import java.util.Map;

import com.apicatalog.linkedtree.LinkedContainer;
import com.apicatalog.linkedtree.builder.TreeBuilderContext;
import com.apicatalog.linkedtree.link.Link;
import com.apicatalog.linkedtree.reader.LinkedReaderError;

public interface TypeAdapter {

    Type adopt(
            Link id,
            Collection<String> types,
            Map<String, LinkedContainer> properties,
            TreeBuilderContext ctx
            ) throws LinkedReaderError;    
}
