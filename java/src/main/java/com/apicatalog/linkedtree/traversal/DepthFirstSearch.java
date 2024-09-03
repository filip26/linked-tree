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

    public static void postOrder(String term, LinkedNode source, Consumer<LinkedContainer> consumer) {
        postOrder(node -> node.isFragment()
                && node.asFragment().terms().contains(term),
                source,
                node -> consumer.accept(node.asFragment().property(term)));
    }

    public static void postOrder(
            final Predicate<LinkedNode> selector,
            final LinkedNode source,
            final Consumer<LinkedNode> consumer) {

        if (source.isContainer()) {
            for (final LinkedNode node : source.asContainer().nodes()) {
                preOrder(selector, node, consumer);
            }
        }

        if (source.isFragment()) {
            for (String term : source.asFragment().terms()) {
                preOrder(selector, source.asFragment().property(term), consumer);
            }
        }

        if (selector.test(source)) {
            consumer.accept(source);
        }
    }

    public static void preOrder(String term, LinkedNode source, Consumer<LinkedContainer> consumer) {
        preOrder(node -> node.isFragment()
                && node.asFragment().terms().contains(term),
                source,
                node -> consumer.accept(node.asFragment().property(term)));
    }

    public static void preOrder(
            final Predicate<LinkedNode> selector,
            final LinkedNode source,
            final Consumer<LinkedNode> consumer) {

        if (selector.test(source)) {
            consumer.accept(source);
        }

        if (source.isFragment()) {
            for (String term : source.asFragment().terms()) {
                preOrder(selector, source.asFragment().property(term), consumer);
            }
        }

        if (source.isContainer()) {
            for (final LinkedNode node : source.asContainer().nodes()) {
                preOrder(selector, node, consumer);
            }
        }
    }
}
