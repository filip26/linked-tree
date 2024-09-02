package com.apicatalog.linkedtree.traversal;

import java.util.function.Consumer;

import com.apicatalog.linkedtree.LinkedContainer;
import com.apicatalog.linkedtree.LinkedNode;

public class DepthFirstSearch {

    public static void postOrder(String term, LinkedContainer container, Consumer<LinkedContainer> consumer) {

        for (final LinkedNode node : container.nodes()) {
            if (node.isContainer()) {
                postOrder(term, node.asContainer(), consumer);
            }
            if (node.isFragment()
                    && node.asFragment().terms().contains(term)) {
                consumer.accept(node.asFragment().property(term));
            }
        }

        if (container.isFragment()
                && container.asFragment().terms().contains(term)) {
            consumer.accept(container.asFragment().property(term));
        }
    }

    public static LinkedContainer findFirstPostOrder(String term, LinkedContainer container) {

        for (final LinkedNode node : container.nodes()) {
            if (node.isContainer()) {
                final LinkedContainer found = findFirstPostOrder(term, node.asContainer());
                if (found != null) {
                    return found;
                }
            }
            if (node.isFragment()
                    && node.asFragment().terms().contains(term)) {
                return node.asFragment().property(term);
            }
        }

        if (container.isFragment()
                && container.asFragment().terms().contains(term)) {
            return container.asFragment().property(term);
        }

        return null;
    }

}
