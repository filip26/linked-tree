package com.apicatalog.linkedtree.traversal;

import com.apicatalog.linkedtree.LinkedContainer;
import com.apicatalog.linkedtree.LinkedNode;

public class NodeSearch {

    public static LinkedContainer findFirst(String term, LinkedContainer container) {

        if (container.isFragment()
                && container.asFragment().terms().contains(term)) {
            return container.asFragment().container(term);
        }

        for (final LinkedNode node : container.nodes()) {

            if (node.isFragment()
                    && node.asFragment().terms().contains(term)) {
                return node.asFragment().container(term);
            }

            if (node.isContainer()) {
                final LinkedContainer found = findFirst(term, node.asContainer());
                if (found != null) {
                    return found;
                }
            }
        }
        return null;
    }

}
