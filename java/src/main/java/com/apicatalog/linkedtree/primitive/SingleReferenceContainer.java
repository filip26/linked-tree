package com.apicatalog.linkedtree.primitive;

import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Objects;

import com.apicatalog.linkedtree.LinkedContainer;
import com.apicatalog.linkedtree.LinkedFragment;
import com.apicatalog.linkedtree.LinkedNode;
import com.apicatalog.linkedtree.LinkedTree;
import com.apicatalog.linkedtree.link.Link;
import com.apicatalog.linkedtree.pi.ProcessingInstruction;
import com.apicatalog.linkedtree.type.Type;

public record SingleReferenceContainer(
        Link id,
        ContainerType containerType,
        LinkedTree root,
        Map<Integer, Collection<ProcessingInstruction>> ops) implements LinkedFragment, LinkedContainer {

    public SingleReferenceContainer {
        Objects.requireNonNull(id);
        Objects.requireNonNull(containerType);
    }

    @Override
    public Collection<LinkedNode> nodes() {
        return List.of(this);
    }

    @Override
    public Type type() {
        return Type.empty();
    }

    @Override
    public Collection<String> terms() {
        return Collections.emptySet();
    }

    @Override
    public LinkedContainer container(String term) {
        return null;
    }

    @Override
    public Collection<ProcessingInstruction> pi(int processingOrder) {
        return ops != null
                ? ops.getOrDefault(processingOrder, Collections.emptyList())
                : Collections.emptyList();
    }

    @Override
    public int hashCode() {
        return Objects.hash(id);
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj == null)
            return false;
        if (getClass() != obj.getClass())
            return false;
        SingleReferenceContainer other = (SingleReferenceContainer) obj;
        return Objects.equals(id, other.id);
    }

    @Override
    public String toString() {
        return "SingleReferenceContainer [id=" + id + ", ops=" + ops.size() + "]";
    }
}
