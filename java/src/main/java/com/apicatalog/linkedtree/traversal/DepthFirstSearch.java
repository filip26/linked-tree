package com.apicatalog.linkedtree.traversal;

import java.util.function.Consumer;

import com.apicatalog.linkedtree.LinkedContainer;
import com.apicatalog.linkedtree.LinkedNode;
import com.apicatalog.linkedtree.traversal.NodeConsumer.IndexScope;

public class DepthFirstSearch {

    public static void postOrder(LinkedNode source, NodeConsumer consumer) {
        postOrder(source, IndexScope.Root, -1, null, 0, consumer);
    }

    static void postOrder(
            final LinkedNode source,
            final IndexScope indexType,
            final int order,
            final String term,
            final int depth,
            final NodeConsumer consumer) {

        if (source.isContainer()) {
            int nodeOrder = 0;
            for (var node : source.asContainer()) {
                postOrder(
                        node,
                        IndexScope.Container,
                        nodeOrder++,
                        null,
                        depth + 1,
                        consumer);
            }
        }
        if (source.isFragment()) {
            for (var property : source.asFragment().terms()) {
                postOrder(
                        source.asFragment().property(property),
                        IndexScope.Fragment,
                        -1,
                        property,
                        depth + 1,
                        consumer);
            }
        }
        consumer.accept(source, indexType, order, term, depth);
    }

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
}
