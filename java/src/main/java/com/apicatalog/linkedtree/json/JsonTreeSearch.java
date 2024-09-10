package com.apicatalog.linkedtree.json;

import java.util.function.Consumer;

import com.apicatalog.linkedtree.LinkedTreeError;
import com.apicatalog.linkedtree.traversal.NodeConsumer;
import com.apicatalog.linkedtree.traversal.NodeSelector;
import com.apicatalog.linkedtree.traversal.NodeSelector.ProcessingPolicy;

import jakarta.json.JsonValue;
import jakarta.json.JsonValue.ValueType;

public class JsonTreeSearch<T> {

    public static void postOrder(NodeSelector<JsonValue> selector, JsonValue source, NodeConsumer<JsonValue> consumer) throws LinkedTreeError {
        postOrder(
                selector,
                source,
                -1,
                null,
                0,
                consumer);
    }

    public static void postOrder(JsonValue source, NodeConsumer<JsonValue> consumer) throws LinkedTreeError {
        postOrder((JsonValue node,
                int indexOrder,
                String indexTerm,
                int depth) -> ProcessingPolicy.Accept,
                source,
                consumer);
    }

    public static void postOrder(String term, JsonValue source, Consumer<JsonValue> consumer) throws LinkedTreeError {
        postOrder(source, (node, indexOrder, indexTerm, depth) -> consumer.accept(node));

    }

    protected static void postOrder(
            final NodeSelector<JsonValue> selector,
            final JsonValue source,
            final int order,
            final String term,
            final int depth,
            final NodeConsumer<JsonValue> consumer) throws LinkedTreeError {

        final ProcessingPolicy policy = selector.test(source, order, term, depth);

        if (ProcessingPolicy.Drop == policy) {
            return;
        }

        if (ProcessingPolicy.Stop == policy) {
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
                postOrder(
                        selector,
                        property.getValue(),
                        -1,
                        property.getKey(),
                        depth + 1,
                        consumer);
            }    
        }
        
        if (ProcessingPolicy.Accept == policy) {
            consumer.accept(source, order, term, depth);
        }
    }
}
