package com.apicatalog.linkedtree.json;

import java.util.Map;
import java.util.Stack;

import com.apicatalog.linkedtree.LinkedTree;
import com.apicatalog.linkedtree.builder.TreeBuilder;
import com.apicatalog.linkedtree.builder.TreeBuilderError;
import com.apicatalog.linkedtree.literal.adapter.LiteralAdapter;
import com.apicatalog.linkedtree.traversal.NodeConsumer;
import com.apicatalog.linkedtree.traversal.NodeSelector;
import com.apicatalog.linkedtree.type.TypeAdapter;

import jakarta.json.JsonStructure;
import jakarta.json.JsonValue;

public abstract class JsonTreeReader extends TreeBuilder<JsonValue> implements NodeConsumer<JsonValue>, NodeSelector<JsonValue> {

    protected NodeSelector<JsonValue> nodeSelector;

    protected JsonTreeReader(
            Map<String, TypeAdapter> typeAdapters,
            Map<String, LiteralAdapter> literalAdapters) {
        super(typeAdapters, literalAdapters);
        this.nodeSelector = null;

    }

    public LinkedTree read(JsonStructure source, NodeSelector<JsonValue> selector) throws TreeBuilderError {
        nodeStack = new Stack<>();
        trees = new Stack<>();

        nodeSelector = selector;

        JsonTreeSearch.postOrder(this, source, this);

        return trees.isEmpty() ? null : trees.peek();
    }

    @Override
    public ProcessingPolicy test(
            JsonValue node,
            int indexOrder,
            String indexTerm,
            int depth) throws TreeBuilderError {

        var policy = nodeSelector.test(node, indexOrder, indexTerm, depth);

        switch (policy) {
        case Accept, Stop -> process(node);
        case Drop, Ignore -> {
        }
        default -> throw new IllegalArgumentException("Unexpected value: " + policy);
        }

        return policy;
    }

    protected abstract void process(JsonValue source) throws TreeBuilderError;
}
