package com.apicatalog.linkedtree.orm.context;

import java.util.Collection;

public record ContextDefinition(
        String id,
        int position,
        Collection<String> includes
        ) {

        
}
