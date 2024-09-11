package com.apicatalog.linkedtree.adapter.resolver;

import java.util.Collection;
import java.util.Objects;

import com.apicatalog.linkedtree.adapter.LinkedFragmentAdapter;

@Deprecated
public record ListAdapterResolver(
        Collection<FragmentAdapterResolver> resolvers) implements FragmentAdapterResolver {

    @Override
    public LinkedFragmentAdapter resolve(String id, Collection<String> types) {
        return resolvers
                .stream()
                .map(resolver -> resolver.resolve(id, types))
                .filter(Objects::nonNull)
                .findFirst()
                .orElse(null);
    }

    public ListAdapterResolver add(FragmentAdapterResolver resolver) {
        this.resolvers.add(resolver);
        return this;
    }

}
