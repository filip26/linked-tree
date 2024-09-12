package com.apicatalog.linkedtree.traversal;

import java.util.function.Consumer;

import com.apicatalog.linkedtree.LinkedNode;
import com.apicatalog.linkedtree.builder.TreeBuilderError;
import com.apicatalog.linkedtree.traversal.NodeSelector.ProcessingPolicy;

public class DepthFirstSearch {

    public static void postOrder(NodeSelector<LinkedNode> selector, LinkedNode source, NodeConsumer<LinkedNode> consumer) throws TreeBuilderError {
        postOrder(
                selector,
                source,
                -1,
                null,
                0,
                consumer);
    }

    public static void postOrder(LinkedNode source, NodeConsumer<LinkedNode> consumer) throws TreeBuilderError {
        postOrder((LinkedNode node,
                int indexOrder,
                String indexTerm,
                int depth) -> ProcessingPolicy.Accept,
                source,
                consumer);
    }

    public static void postOrder(String term, LinkedNode source, Consumer<LinkedNode> consumer) throws TreeBuilderError {
        postOrder(source, (node, indexOrder, indexTerm, depth) -> consumer.accept(node));

    }

    public static void postOrder(String term, LinkedNode source, NodeConsumer<LinkedNode> consumer) throws TreeBuilderError {
        postOrder((LinkedNode node,
                int indexOrder,
                String indexTerm,
                int depth) -> node.isFragment()
                        && node.asFragment().terms().contains(term)
                                ? ProcessingPolicy.Accept
                                : ProcessingPolicy.Ignore,
                source,
                (LinkedNode node,
                        int indexOrder,
                        String indexTerm,
                        int depth) -> consumer.accept(
                                node.asFragment().container(term),
                                indexOrder,
                                indexTerm,
                                depth));
    }

    protected static void postOrder(
            final NodeSelector<LinkedNode> selector,
            final LinkedNode source,
            final int order,
            final String term,
            final int depth,
            final NodeConsumer<LinkedNode> consumer) throws TreeBuilderError {

        final ProcessingPolicy policy = selector.test(source, order, term, depth);

        if (ProcessingPolicy.Drop.equals(policy)) {
            return;
        }

        if (ProcessingPolicy.Stop.equals(policy)) {
            consumer.accept(source, order, term, depth);
            return;
        }

        if (source.isContainer()) {
            int nodeOrder = 0;
            for (var node : source.asContainer()) {
                postOrder(
                        selector,
                        node,
                        nodeOrder++,
                        null,
                        depth + 1,
                        consumer);
            }
        }
        if (source.isFragment()) {
            for (var property : source.asFragment().terms()) {
                postOrder(
                        selector,
                        source.asFragment().container(property),
                        -1,
                        property,
                        depth + 1,
                        consumer);
            }
        }
        if (ProcessingPolicy.Accept.equals(policy)) {
            consumer.accept(source, order, term, depth);
        }
    }

    public static void preOrder(NodeSelector<LinkedNode> selector, LinkedNode source, NodeConsumer<LinkedNode> consumer) throws TreeBuilderError {
        preOrder(
                selector,
                source,
                -1,
                null,
                0,
                consumer);
    }

    public static void preOrder(LinkedNode source, NodeConsumer<LinkedNode> consumer) throws TreeBuilderError {
        preOrder((LinkedNode node,
                int indexOrder,
                String indexTerm,
                int depth) -> ProcessingPolicy.Accept,
                source,
                consumer);
    }

    public static void preOrder(String term, LinkedNode source, Consumer<LinkedNode> consumer) throws TreeBuilderError {
        preOrder(source, (node, indexOrder, indexTerm, depth) -> consumer.accept(node));
    }

    public static void preOrder(
            final String term,
            final LinkedNode source,
            final NodeConsumer<LinkedNode> consumer) throws TreeBuilderError {
        preOrder((LinkedNode node,
                int indexOrder,
                String indexTerm,
                int depth) -> node.isFragment()
                        && node.asFragment().terms().contains(term)
                                ? ProcessingPolicy.Accept
                                : ProcessingPolicy.Ignore,
                source,
                (LinkedNode node,
                        int indexOrder,
                        String indexTerm,
                        int depth) -> consumer.accept(
                                node.asFragment().container(term),
                                indexOrder,
                                indexTerm,
                                depth));
    }

    protected static void preOrder(
            final NodeSelector<LinkedNode> selector,
            final LinkedNode source,
            final int order,
            final String term,
            final int depth,
            final NodeConsumer<LinkedNode> consumer) throws TreeBuilderError {

        final ProcessingPolicy policy = selector.test(source, order, term, depth);

        if (ProcessingPolicy.Drop.equals(policy)) {
            return;

        } else if (ProcessingPolicy.Accept.equals(policy)) {
            consumer.accept(source, order, term, depth);

        } else if (ProcessingPolicy.Stop.equals(policy)) {
            consumer.accept(source, order, term, depth);
            return;
        }

        if (source.isFragment()) {
            for (var property : source.asFragment().terms()) {
                preOrder(
                        selector,
                        source.asFragment().container(property),
                        -1,
                        property,
                        depth + 1,
                        consumer);
            }
        }

        if (source.isContainer()) {
            int nodeOrder = 0;
            for (var node : source.asContainer()) {
                preOrder(
                        selector,
                        node,
                        nodeOrder++,
                        null,
                        depth + 1,
                        consumer);
            }
        }
    }
}
