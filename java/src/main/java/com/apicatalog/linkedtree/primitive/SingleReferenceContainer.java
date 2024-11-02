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
import com.apicatalog.linkedtree.type.FragmentType;

public final class SingleReferenceContainer implements LinkedFragment, LinkedContainer {

    protected final Link id;
    protected final Collection<LinkedNode> nodes;
    protected final LinkedTree root;
    protected final Map<Integer, Collection<ProcessingInstruction>> ops;

    protected SingleReferenceContainer(
            Link id,
            LinkedTree root,
            Map<Integer, Collection<ProcessingInstruction>> ops) {
        this.id = id;
        this.nodes = List.of(this);
        this.root = root;
        this.ops = ops;
    }

    public static final SingleReferenceContainer of(Link id, LinkedTree root, Map<Integer, Collection<ProcessingInstruction>> ops) {
        return new SingleReferenceContainer(id, root, ops);
    }

    @Override
    public LinkedNode node() {
        return this;
    }

    @Override
    public Collection<LinkedNode> nodes() {
        return nodes;
    }

    @Override
    public FragmentType type() {
        return FragmentType.empty();
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

    @Override
    public LinkedTree root() {
        return root;
    }

    @Override
    public Link id() {
        return id;
    }
}
