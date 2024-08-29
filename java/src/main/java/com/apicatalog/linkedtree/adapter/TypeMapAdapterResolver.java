package com.apicatalog.linkedtree.adapter;

import java.util.Collection;
import java.util.LinkedHashMap;
import java.util.Map;

import com.apicatalog.linkedtree.selector.StringValueSelector;

public record TypeMapAdapterResolver(
        Map<String, LinkedFragmentAdapter> typeMap) implements FragmentAdapterResolver {

    public static final Builder create() {
        return new Builder();
    }

    @Override
    public LinkedFragmentAdapter resolve(String id, Collection<String> types, StringValueSelector stringSelector) {
        return typeMap.keySet()
                .stream()
                .filter(types::contains)
                .findFirst()
                .map(typeMap::get)
                .orElse(null);
    }

    static class Builder {

        protected Map<String, LinkedFragmentAdapter> typeMap;

        public Builder() {
            this.typeMap = new LinkedHashMap<>();
        }

        public Builder add(String type, LinkedFragmentReader reader) {
            this.typeMap.put(type, () -> reader);
            return this;
        }

        public TypeMapAdapterResolver build() {
            return new TypeMapAdapterResolver(typeMap);
        }

    }

}
