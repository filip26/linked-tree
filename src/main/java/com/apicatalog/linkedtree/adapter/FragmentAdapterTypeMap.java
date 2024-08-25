package com.apicatalog.linkedtree.adapter;

import java.util.Collection;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Optional;

import com.apicatalog.linkedtree.LinkedContainer;
import com.apicatalog.linkedtree.LinkedFragment;
import com.apicatalog.linkedtree.link.Link;

public record FragmentAdapterTypeMap(
        Map<String, LinkedFragmentReader> typeMap
        ) implements LinkedFragmentAdapter {

    public static class Builder {
        
        protected Map<String, LinkedFragmentReader> typeMap;
        
        public Builder() {
            this.typeMap = new LinkedHashMap<>();
        }
        
        public Builder add(String type, LinkedFragmentReader reader) {
            this.typeMap.put(type, reader);
            return this;
        }
        
        public FragmentAdapterTypeMap build() {
            return new FragmentAdapterTypeMap(typeMap);
        }
        
    }
    
    public static final Builder create() {
        return new Builder();
    }
    
    @Override
    public boolean accepts(String id, Collection<String> types) {
        return types.stream().anyMatch(typeMap::containsKey);
    }

    @Override
    public LinkedFragment read(Link id, Collection<String> types, Map<String, LinkedContainer> properties, Object meta) {

        Optional<LinkedFragmentReader> reader = types.stream().filter(typeMap::containsKey).map(typeMap::get).findFirst();

        if (reader.isPresent()) {
            return reader.get().read(id, types, properties, meta);
        }
        
        return null;
    }

}
