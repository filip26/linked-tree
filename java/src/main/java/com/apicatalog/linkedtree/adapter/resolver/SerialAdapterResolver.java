package com.apicatalog.linkedtree.adapter.resolver;

import java.util.Collection;
import java.util.Objects;

import com.apicatalog.linkedtree.fragment.FragmentAdapterResolver;
import com.apicatalog.linkedtree.fragment.LinkedFragmentAdapter;

@Deprecated
public record SerialAdapterResolver(
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

    public SerialAdapterResolver add(FragmentAdapterResolver resolver) {
        this.resolvers.add(resolver);
        return this;
    }

}
