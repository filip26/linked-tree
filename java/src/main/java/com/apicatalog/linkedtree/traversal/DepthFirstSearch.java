package com.apicatalog.linkedtree.traversal;

import java.util.function.Consumer;
import java.util.function.Predicate;

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
        postOrder(node -> node.isFragment()
                && node.asFragment().terms().contains(term),
                container,
                node -> consumer.accept(node.asFragment().property(term)));
    }

    public static void postOrder(
            final Predicate<LinkedNode> selector,
            final LinkedContainer container,
            final Consumer<LinkedNode> consumer) {

        for (final LinkedNode node : container.nodes()) {

            if (node.isContainer()) {
                preOrder(selector, node.asContainer(), consumer);
            }

            if (node.isFragment()) {
                for (String term : node.asFragment().terms()) {
                    preOrder(selector, node.asFragment().property(term), consumer);
                }
            }

            if (selector.test(node)) {
                consumer.accept(node);
            }
        }

        if (selector.test(container)) {
            consumer.accept(container);
        }
    }

    public static void preOrder(String term, LinkedContainer container, Consumer<LinkedContainer> consumer) {
        preOrder(node -> node.isFragment()
                && node.asFragment().terms().contains(term),
                container,
                node -> consumer.accept(node.asFragment().property(term)));
    }

    public static void preOrder(
            final Predicate<LinkedNode> selector,
            final LinkedContainer container,
            final Consumer<LinkedNode> consumer) {

        if (selector.test(container)) {
            consumer.accept(container);
        }

        for (final LinkedNode node : container.nodes()) {

            if (selector.test(node)) {
                consumer.accept(node);
            }

            if (node.isFragment()) {
                for (String term : node.asFragment().terms()) {
                    preOrder(selector, node.asFragment().property(term), consumer);
                }
            }

            if (node.isContainer()) {
                preOrder(selector, node.asContainer(), consumer);
            }

        }
    }
}
