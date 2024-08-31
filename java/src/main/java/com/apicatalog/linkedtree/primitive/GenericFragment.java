package com.apicatalog.linkedtree.primitive;

import java.util.Collection;
import java.util.Map;
import java.util.function.Supplier;

import com.apicatalog.linkedtree.Link;
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
}
