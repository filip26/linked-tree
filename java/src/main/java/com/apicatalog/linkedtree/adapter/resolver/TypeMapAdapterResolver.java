package com.apicatalog.linkedtree.adapter.resolver;

import java.util.Collection;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;

import com.apicatalog.linkedtree.adapter.LinkedFragmentAdapter;
import com.apicatalog.linkedtree.reader.LinkedFragmentReader;

public record TypeMapAdapterResolver(
        Map<String, FragmentAdapterResolver> typeMap) implements FragmentAdapterResolver {

    public static final Builder create() {
        return new Builder();
    }

    @Override
    public LinkedFragmentAdapter resolve(String id, Collection<String> types) {
        return typeMap.keySet()
                .stream()
                .filter(types::contains)
                .map(typeMap::get)
                .map(resolver -> resolver.resolve(id, types))
                .filter(Objects::nonNull)
                .findFirst()
                .orElse(null);
    }

    public static class Builder {

        protected Map<String, FragmentAdapterResolver> typeMap;

        public Builder() {
            this.typeMap = new LinkedHashMap<>();
        }

        public Builder add(String type, LinkedFragmentReader reader) {
            return add(type, () -> reader);
        }

        public Builder add(String type, LinkedFragmentAdapter adapter) {
            return add(type, (id, types) -> adapter);
        }

        public Builder add(String type, FragmentAdapterResolver resolver) {

            var previous = typeMap.get(type);
            if (previous == null) {
                typeMap.put(type, resolver);
                return this;
            }

            if (previous instanceof ListAdapterResolver list) {
                list.add(resolver);
                return this;
            }

            typeMap.put(type,
                    new ListAdapterResolver(List.of(
                            previous,
                            resolver)));
            return this;
        }

        public TypeMapAdapterResolver build() {
            return new TypeMapAdapterResolver(typeMap);
        }

    }

}
