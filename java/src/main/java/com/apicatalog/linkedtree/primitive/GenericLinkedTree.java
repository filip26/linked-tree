package com.apicatalog.linkedtree.primitive;

import java.util.Collection;
import java.util.Collections;
import java.util.Map;

import com.apicatalog.linkedtree.LinkedContainer;
import com.apicatalog.linkedtree.LinkedNode;
import com.apicatalog.linkedtree.LinkedTree;
import com.apicatalog.linkedtree.link.Link;
import com.apicatalog.linkedtree.pi.ProcessingInstruction;

public record GenericLinkedTree(
        Link id,
        Collection<String> type,
        Map<String, LinkedContainer> entries,
        Collection<LinkedNode> nodes,
        Map<String, Link> linkMap,
        Map<LinkedNode, Collection<ProcessingInstruction>> opsMap
        ) implements LinkedTree {

    public static GenericLinkedTree of(Collection<LinkedNode> nodes, Map<String, Link> links, Map<LinkedNode, Collection<ProcessingInstruction>> opsMap) {
        return new GenericLinkedTree(null, Collections.emptySet(), Collections.emptyMap(), nodes, links, opsMap);
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
    public Collection<ProcessingInstruction> pi(LinkedNode node) {
        return opsMap != null
                ? opsMap.getOrDefault(node, Collections.emptyList())
                : Collections.emptyList();
    }
}
