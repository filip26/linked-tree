package com.apicatalog.linkedtree.primitive;

import java.util.Collection;
import java.util.Collections;
import java.util.Map;
import java.util.Objects;

import com.apicatalog.linkedtree.LinkedContainer;
import com.apicatalog.linkedtree.LinkedNode;
import com.apicatalog.linkedtree.LinkedTree;
import com.apicatalog.linkedtree.link.Link;
import com.apicatalog.linkedtree.pi.ProcessingInstruction;
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

//    public static GenericTree of(Collection<LinkedNode> nodes, Map<String, Link> links, Collection<LinkedTree> subtrees, LinkedTree root,Map<Integer, Collection<ProcessingInstruction>> opsMap) {
//        return new GenericTree(null, Type.empty(), Collections.emptyMap(), nodes, links, subtrees, root, opsMap);
//    }
//
//    public static GenericTree of(LinkedNode node, Map<String, Link> links, Collection<LinkedTree> subtrees, LinkedTree root, Map<Integer, Collection<ProcessingInstruction>> opsMap) {
//        return new GenericTree(null, Type.empty(), Collections.emptyMap(), List.of(node), links, subtrees, root, opsMap);
//    }
    
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
        return Objects.hash(entries, id, nodes, ops, type);
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj == null)
            return false;
        if (getClass() != obj.getClass())
            return false;
        GenericTree other = (GenericTree) obj;
        return Objects.equals(entries, other.entries) && Objects.equals(id, other.id) && Objects.equals(nodes, other.nodes) && Objects.equals(ops, other.ops) && Objects.equals(type, other.type);
    }

    @Override
    public String toString() {
        return "GenericTree [id=" + id + ", type=" + type + ", entries=" + entries.size() + ", nodes=" + nodes.size() + ", ops=" + ops.size() + "]";
    }
    
}
