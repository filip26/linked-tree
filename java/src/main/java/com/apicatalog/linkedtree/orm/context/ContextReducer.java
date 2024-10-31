package com.apicatalog.linkedtree.orm.context;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.Objects;

public class ContextReducer {

    Map<String, ContextDefinition> defs;
    
    public ContextReducer() {
        this.defs = new HashMap<>();
    }
    
    public Collection<String> reduce(Collection<String> context) {
        
        Objects.requireNonNull(context);

        if (context.size() < 2) {
            return context;
        }
        
        Collection<String> reduced = new ArrayList<>(context.size());
        ContextDefinition[] prev = new ContextDefinition[context.size()];
        int index = 0;
        
        for (String ctx : context) {
            if (index == 0 || !contains(prev, index, ctx)) {
                reduced.add(ctx);
            }
            prev[index++] = defs.get(ctx);
        }                
        
        return reduced;
    }
    
    static boolean contains(ContextDefinition[] defs, int length, String context) {
        for (int i = 0; i < length; i++) {
            if (defs[i] != null && defs[i].includes().contains(context)) {
                return true;
            }
        }
        return false;
    }
    
    public ContextReducer define(
            String id,
            int position,
            Collection<String> includes) {

        this.defs.put(id, new ContextDefinition(id, position, includes));
        return this;
    }

    public ContextReducer define(
            String id,
            Collection<String> includes) {
        return define(id, -1, includes);
    }

}
