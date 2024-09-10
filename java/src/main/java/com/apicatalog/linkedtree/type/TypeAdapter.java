package com.apicatalog.linkedtree.type;

import java.util.Collection;
import java.util.Map;

import com.apicatalog.linkedtree.LinkedContainer;
import com.apicatalog.linkedtree.LinkedNode;
import com.apicatalog.linkedtree.builder.TreeBuilderContext;
import com.apicatalog.linkedtree.link.Link;
import com.apicatalog.linkedtree.reader.LinkedReaderError;

public interface TypeAdapter {

    Type typeOf(
            Link id,
            Collection<String> types,
            Map<String, LinkedContainer> properties,
            TreeBuilderContext ctx
            ) throws LinkedReaderError;
    

    /**
     * Creates a new object instance initialized by values found in the given
     * {@link LinkedNode}.
     * 
     * @param node
     * @return a new instance
     * @throws TypeAdapterError
     */
    <T> T adapt(LinkedNode node) throws TypeAdapterError;

    Class<?> typeInterface(); 
//    {
//        return Object.class;
//    }
}
