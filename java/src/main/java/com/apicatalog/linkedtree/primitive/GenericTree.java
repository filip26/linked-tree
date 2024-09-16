package com.apicatalog.linkedtree.primitive;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.function.Function;
import java.util.stream.Collectors;

import com.apicatalog.linkedtree.LinkedContainer;
import com.apicatalog.linkedtree.LinkedNode;
import com.apicatalog.linkedtree.LinkedTree;
import com.apicatalog.linkedtree.builder.GenericTreeCloner;
import com.apicatalog.linkedtree.link.Link;
import com.apicatalog.linkedtree.link.MutableLink;
import com.apicatalog.linkedtree.pi.ProcessingInstruction;
import com.apicatalog.linkedtree.type.AdaptableType;
import com.apicatalog.linkedtree.type.Type;

public record GenericTree(
        Link id,
        Type type,
        Map<String, LinkedContainer> entries,
        Collection<LinkedNode> nodes,
        Map<String, Link> linkMap,
        Collection<LinkedTree> subtrees,
        LinkedTree root,
        Map<Integer, Collection<ProcessingInstruction>> ops) implements LinkedTree {

    @Override
    public Collection<String> terms() {
        return entries.keySet();
    }

    @Override
    public LinkedContainer container(String term) {
        return entries.get(term);
    }

    @Override
    public Collection<Link> links() {
        return linkMap.values();
    }

    @Override
    public Collection<ProcessingInstruction> pi(int processingOrder) {
        return ops != null
                ? ops.getOrDefault(processingOrder, Collections.emptyList())
                : Collections.emptyList();
    }

    @Override
    public int hashCode() {
        return System.identityHashCode(this);
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        return (obj != null);
    }

    @Override
    public String toString() {
        return "GenericTree [id=" + id + ", type=" + type + ", entries=" + entries.size() + ", nodes=" + nodes.size() + ", ops=" + ops.size() + "]";
    }

    public static GenericTree of(LinkedTree source, Link id, LinkedTree root) {
        // clone links
        final Map<String, Link> links = source.asTree().links().isEmpty()
                ? Collections.emptyMap()
                : source.asTree().links()
                        .stream()
                        .map(Link::uri)
                        .collect(Collectors.toMap(
                                Function.identity(),
                                MutableLink::of));

        var types = source.asTree().type().isEmpty()
                ? Type.empty()
                : AdaptableType.of(source.asTree().type().stream().toList());

        final Map<String, LinkedContainer> treeMeta =
//        source.asFragment().terms().isEmpty()
//                ? Collections.emptyMap()
//                : 
                new LinkedHashMap<>(source.asFragment().terms().size());

        var tree = new GenericTree(
                id,

                types,

                treeMeta,

//                source.asContainer().nodes().isEmpty()
//                        ? Collections.emptyList()
//                        : 
                new ArrayList<>(source.asContainer().size()),

                links,

//                source.asTree().subtrees().isEmpty()
//                        ? Collections.emptyList()
//                        : 
                new ArrayList<>(source.asTree().subtrees().size()),

                root,
                GenericTreeCloner.cloneOps(source.asContainer()));

        if (types instanceof AdaptableType adaptableType) {
            adaptableType.node(tree);
        }

        return tree;
    }

}
