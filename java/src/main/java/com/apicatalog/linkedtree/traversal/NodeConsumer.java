package com.apicatalog.linkedtree.traversal;

import com.apicatalog.linkedtree.LinkedNode;

@FunctionalInterface
public interface NodeConsumer {

    enum IndexScope {
        Container,
        Fragment,
        Root
    }

    void accept(
            LinkedNode node,
            IndexScope indexType,
            int order,
            String term,
            int depth);

//    public static NodeIndex container(LinkedContainer container, int index) {
//        return new NodeIndex(container, Type.ContainerIndex, index, null);
//    }
//
//    public static NodeIndex fragment(LinkedFragment fragment, String term) {
//        return new NodeIndex(fragment, Type.FragmentIndex, -1, term);
//    }
}
