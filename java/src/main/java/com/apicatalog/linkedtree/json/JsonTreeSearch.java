package com.apicatalog.linkedtree.json;

import java.util.function.Consumer;

import com.apicatalog.linkedtree.builder.TreeBuilderError;
import com.apicatalog.linkedtree.traversal.NodeConsumer;
import com.apicatalog.linkedtree.traversal.NodeSelector;
import com.apicatalog.linkedtree.traversal.NodeSelector.TraversalPolicy;

import jakarta.json.JsonValue;
import jakarta.json.JsonValue.ValueType;

public class JsonTreeSearch<T> {

    public static void postOrder(NodeSelector<JsonValue> selector, JsonValue source, NodeConsumer<JsonValue> consumer) throws TreeBuilderError {
        postOrder(
                selector,
                source,
                -1,
                null,
                0,
                consumer);
    }

    public static void postOrder(JsonValue source, NodeConsumer<JsonValue> consumer) throws TreeBuilderError {
        postOrder((JsonValue node,
                int indexOrder,
                String indexTerm,
                int depth) -> TraversalPolicy.Accept,
                source,
                consumer);
    }

    public static void postOrder(String term, JsonValue source, Consumer<JsonValue> consumer) throws TreeBuilderError {
        postOrder(source, (node, indexOrder, indexTerm, depth) -> consumer.accept(node));

    }

    protected static void postOrder(
            final NodeSelector<JsonValue> selector,
            final JsonValue source,
            final int order,
            final String term,
            final int depth,
            final NodeConsumer<JsonValue> consumer) throws TreeBuilderError {

        final TraversalPolicy policy = selector.test(source, order, term, depth);

        if (TraversalPolicy.Drop == policy) {
            return;
        }

        if (TraversalPolicy.Stop == policy) {
            consumer.accept(source, order, term, depth);
            return;
        }

        if (ValueType.ARRAY == source.getValueType()) {
            int nodeOrder = 0;
            for (var node : source.asJsonArray()) {
                postOrder(
                        selector,
                        node,
                        nodeOrder++,
                        null,
                        depth + 1,
                        consumer);
            }
        } else if (ValueType.OBJECT == source.getValueType()) {

            for (var property : source.asJsonObject().entrySet()) {
                try {
                    postOrder(
                            selector,
                            property.getValue(),
                            -1,
                            property.getKey(),
                            depth + 1,
                            consumer);
                } catch (IllegalArgumentException e) {
                    throw new TreeBuilderError(e, property.getKey());
                }
            }
        }

        if (TraversalPolicy.Accept == policy) {
            consumer.accept(source, order, term, depth);
        }
    }
}
