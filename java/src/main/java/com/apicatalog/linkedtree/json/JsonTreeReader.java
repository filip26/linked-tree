package com.apicatalog.linkedtree.json;

import java.util.Map;
import java.util.Stack;

import com.apicatalog.linkedtree.LinkedTree;
import com.apicatalog.linkedtree.adapter.resolver.FragmentAdapterResolver;
import com.apicatalog.linkedtree.builder.PostOrderTreeBuilder;
import com.apicatalog.linkedtree.reader.LinkedLiteralReader;
import com.apicatalog.linkedtree.traversal.NodeConsumer;
import com.apicatalog.linkedtree.traversal.NodeSelector;

import jakarta.json.JsonStructure;
import jakarta.json.JsonValue;

public abstract class JsonTreeReader extends PostOrderTreeBuilder<JsonValue> implements NodeConsumer<JsonValue>, NodeSelector<JsonValue> {

    protected NodeSelector<JsonValue> nodeSelector;

    protected JsonTreeReader(
            FragmentAdapterResolver fragmentAdapterResolver,
            Map<String, LinkedLiteralReader> literalAdapters) {
        super(fragmentAdapterResolver, literalAdapters);
        this.nodeSelector = null;

    }

    public LinkedTree read(JsonStructure source, NodeSelector<JsonValue> selector) {
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
            int depth) {

        var policy = nodeSelector.test(node, indexOrder, indexTerm, depth);

        switch (policy) {
        case Accept, Stop -> process(node);
        case Drop, Ignore -> {
        }
        default -> throw new IllegalArgumentException("Unexpected value: " + policy);
        }

        return policy;
    }

    protected abstract void process(JsonValue source);
}
