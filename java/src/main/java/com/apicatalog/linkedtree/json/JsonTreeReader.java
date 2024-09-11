package com.apicatalog.linkedtree.json;

import java.util.Map;
import java.util.Stack;

import com.apicatalog.linkedtree.LinkedTree;
import com.apicatalog.linkedtree.adapter.resolver.FragmentAdapterResolver;
import com.apicatalog.linkedtree.builder.TreeBuilderError;
import com.apicatalog.linkedtree.builder.TreeBuilder;
import com.apicatalog.linkedtree.reader.LiteralReader;
import com.apicatalog.linkedtree.traversal.NodeConsumer;
import com.apicatalog.linkedtree.traversal.NodeSelector;

import jakarta.json.JsonStructure;
import jakarta.json.JsonValue;

public abstract class JsonTreeReader extends TreeBuilder<JsonValue> implements NodeConsumer<JsonValue>, NodeSelector<JsonValue> {

    protected NodeSelector<JsonValue> nodeSelector;

    protected JsonTreeReader(
            FragmentAdapterResolver fragmentAdapterResolver,
            Map<String, LiteralReader> literalAdapters) {
        super(fragmentAdapterResolver, literalAdapters);
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
