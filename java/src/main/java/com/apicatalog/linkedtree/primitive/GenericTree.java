package com.apicatalog.linkedtree.primitive;

import java.util.Collection;
import java.util.Collections;
import java.util.Map;

import com.apicatalog.linkedtree.LinkedContainer;
import com.apicatalog.linkedtree.LinkedNode;
import com.apicatalog.linkedtree.LinkedTree;
import com.apicatalog.linkedtree.link.Link;
import com.apicatalog.linkedtree.pi.ProcessingInstruction;

public record GenericTree(
        Link id,
        Collection<String> type,
        Map<String, LinkedContainer> entries,
        Collection<LinkedNode> nodes,
        Map<String, Link> linkMap,
        Map<Integer, Collection<ProcessingInstruction>> opsMap) implements LinkedTree {

    public static GenericTree of(Collection<LinkedNode> nodes, Map<String, Link> links, Map<Integer, Collection<ProcessingInstruction>> opsMap) {
        return new GenericTree(null, Collections.emptySet(), Collections.emptyMap(), nodes, links, opsMap);
    }

    @Override
    public Collection<String> terms() {
        return entries.keySet();
    }

    @Override
    public LinkedContainer property(String term) {
        return entries.get(term);
    }

    @Override
    public Collection<Link> links() {
        return linkMap.values();
    }

    @Override
    public Collection<ProcessingInstruction> pi(int processingOrder) {
        return opsMap != null
                ? opsMap.getOrDefault(processingOrder, Collections.emptyList())
                : Collections.emptyList();
    }
}
