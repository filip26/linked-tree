package com.apicatalog.linkedtree.adapter;

import java.util.Collection;
import java.util.LinkedHashMap;
import java.util.Map;

import com.apicatalog.linkedtree.LinkedContainer;
import com.apicatalog.linkedtree.LinkedFragment;
import com.apicatalog.linkedtree.link.Link;

public record FragmentAdapterTypeMap(
        Map<String, LinkedFragmentReader> typeMap) implements LinkedFragmentAdapter {

    public static final Builder create() {
        return new Builder();
    }

    @Override
    public boolean accepts(String id, Collection<String> types) {
        return types.stream().anyMatch(typeMap::containsKey);
    }

    @Override
    public LinkedFragment read(final Link id, final Collection<String> types, final Map<String, LinkedContainer> properties) {
        return typeMap.keySet()
                .stream()
                .filter(types::contains)
                .findFirst()
                .map(typeMap::get)
                .map(reader -> reader.read(id, types, properties))
                .orElse(null);
    }

    static class Builder {

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

}
