package com.apicatalog.linkedtree.primitive;

import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.function.Supplier;

import com.apicatalog.linkedtree.Link;
import com.apicatalog.linkedtree.LinkedContainer;
import com.apicatalog.linkedtree.LinkedFragment;
import com.apicatalog.linkedtree.LinkedNode;
import com.apicatalog.linkedtree.LinkedTree;
import com.apicatalog.linkedtree.pi.ProcessingInstruction;

public record SingleReferenceContainer(
        Link id,
        Type containerType,
        Supplier<LinkedTree> rootSupplier,
        Supplier<Map<Integer, Collection<ProcessingInstruction>>> ops) implements LinkedFragment, LinkedContainer {

    public SingleReferenceContainer {
        Objects.requireNonNull(id);
        Objects.requireNonNull(containerType);
    }

    @Override
    public Collection<LinkedNode> nodes() {
        return List.of(this);
    }

    @Override
    public Collection<String> type() {
        return Collections.emptySet();
    }

    @Override
    public Collection<String> terms() {
        return Collections.emptySet();
    }

    @Override
    public LinkedContainer property(String term) {
        return null;
    }

    @Override
    public Collection<ProcessingInstruction> pi(int processingOrder) {
        return ops != null && ops.get() != null
                ? ops.get().getOrDefault(processingOrder, Collections.emptyList())
                : Collections.emptyList();
    }

    @Override
    public LinkedTree root() {
        return rootSupplier.get();
    }
}
