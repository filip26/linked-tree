package com.apicatalog.linkedtree.primitive;

import java.util.Collection;
import java.util.Map;
import java.util.Objects;
import java.util.function.Supplier;

import com.apicatalog.linkedtree.Link;
import com.apicatalog.linkedtree.Linkable;
import com.apicatalog.linkedtree.LinkedContainer;
import com.apicatalog.linkedtree.LinkedFragment;
import com.apicatalog.linkedtree.LinkedTree;

public record GenericFragment(
        Link id,
        Collection<String> type,
        Map<String, LinkedContainer> entries,
        Supplier<LinkedTree> rootSupplier) implements LinkedFragment {

    @Override
    public Collection<String> terms() {
        return entries.keySet();
    }

    @Override
    public LinkedContainer property(String term) {
        return entries.get(term);
    }

    @Override
    public LinkedTree root() {
        return rootSupplier.get();
    }

    @Override
    @SuppressWarnings("unchecked")
    public <T> T cast(Class<T> clazz) {
        if (id() != null 
                && id().target() != null
                && !Objects.equals(id().target(), this)) {
            return (T) id().target().cast(clazz);
        }
        return (T) this;
    }

    @Override
    public Linkable cast() {
        if (id() != null 
                && id().target() != null
                && !Objects.equals(id().target(), this)) {
            return id().target().cast();
        }
        return this;
    }

}
