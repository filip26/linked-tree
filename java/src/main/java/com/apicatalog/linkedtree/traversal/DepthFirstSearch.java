package com.apicatalog.linkedtree.traversal;

import java.util.function.Consumer;

import com.apicatalog.linkedtree.LinkedContainer;
import com.apicatalog.linkedtree.LinkedNode;

public class DepthFirstSearch {

    public static void postOrder(String term, LinkedContainer tree, Consumer<LinkedContainer> consumer) {

        for (final LinkedNode node : tree.nodes()) {
            if (node.isContainer()) {
                postOrder(term, node.asContainer(), consumer);

            } else if (node.isFragment()
                    && node.asFragment().terms().contains(term)) {
                consumer.accept(node.asFragment().property(term));
            }
        }
    }
    
}
