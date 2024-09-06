package com.apicatalog.linkedtree.builder;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.Stack;

import com.apicatalog.linkedtree.LinkedContainer;
import com.apicatalog.linkedtree.LinkedContainer.Type;
import com.apicatalog.linkedtree.LinkedNode;
import com.apicatalog.linkedtree.LinkedTree;
import com.apicatalog.linkedtree.adapter.resolver.FragmentAdapterResolver;
import com.apicatalog.linkedtree.link.Link;
import com.apicatalog.linkedtree.link.MutableLink;
import com.apicatalog.linkedtree.primitive.GenericContainer;
import com.apicatalog.linkedtree.primitive.GenericFragment;
import com.apicatalog.linkedtree.primitive.GenericTree;
import com.apicatalog.linkedtree.reader.LinkedLiteralReader;
import com.apicatalog.linkedtree.traversal.NodeConsumer;

public class PostOrderTreeBuilder<T> implements NodeConsumer<T> {

    protected FragmentAdapterResolver fragmentAdapterResolver;
    protected Stack<Map<String, LinkedLiteralReader>> literalAdapters;

    protected Stack<LinkedNode> nodeStack;

    protected Stack<LinkedTree> trees;

    protected PostOrderTreeBuilder(
            FragmentAdapterResolver fragmentAdapterResolver,
            Map<String, LinkedLiteralReader> literalAdapters) {
        this.fragmentAdapterResolver = fragmentAdapterResolver;
        this.literalAdapters = new Stack<>();
        this.literalAdapters.push(literalAdapters);
        this.nodeStack = null;
        this.trees = null;
    }

    @Override
    public void accept(T node, int indexOrder, String indexTerm, int depth) {
        if (indexOrder != -1) {
            bind(indexOrder);

        } else if (indexTerm != null) {
            bind(indexTerm);
        }
    }

    public PostOrderTreeBuilder<T> tree(
            String id,
            Collection<String> type,
            int propertiesCapacity,
            int nodesCapacity) {
        return this;
    }

    public PostOrderTreeBuilder<T> fragment(
            String id,
            Collection<String> type,
            int capacity) {
        return this;
    }

    public PostOrderTreeBuilder<T> container(int capacity) {

        if (capacity == 0) {
            nodeStack.push(LinkedContainer.EMPTY);
            return this;
        }

        // place holder, lazy initialization
        if (capacity == 1) {
            nodeStack.push(null);
            return this;
        }

        nodeStack.push(mutableContainer(Type.UnorderedSet, capacity));
        return this;
    }

    public PostOrderTreeBuilder<T> list(int capacity) {
        return this;
    }

    public PostOrderTreeBuilder<T> bind(String term) {

        LinkedNode child = nodeStack.pop();
        if (child == null) {
            child = LinkedContainer.EMPTY;
        }

        final LinkedNode parent = nodeStack.peek();

        if (parent.isTree()) {
            ((GenericTree) parent.asTree()).entries().put(term, child.asContainer());

        } else if (parent.isFragment()) {
            ((GenericFragment) parent.asFragment()).entries().put(term, child.asContainer());

        } else {
            // TODO fallback, create missing container
            throw new IllegalStateException();
        }
        return subtree(child);
    }

    public PostOrderTreeBuilder<T> bind(int index) {

        LinkedNode child = nodeStack.pop();
        if (child == null) {
            child = LinkedContainer.EMPTY;
        }

        LinkedNode parent = nodeStack.peek();

        if (parent != null) {
            if (parent.isTree()) {
                ((GenericTree) parent.asContainer()).nodes().add(child);

            } else if (parent.isContainer()) {
                ((GenericContainer) parent.asContainer()).nodes().add(child);

            } else {
                throw new IllegalStateException();
            }
            return subtree(child);
        }

        if (child.isTree()) {
            nodeStack.pop();
            nodeStack.push(child);
            return this;

        }

        if (child.isContainer() && LinkedContainer.Type.OrderedList == child.asContainer().containerType()) {
            nodeStack.pop();
            nodeStack.push(child);
            return this;

        }

        nodeStack.pop();
        parent = nodeStack.push(mutableContainer(Type.UnorderedSet, 1));
        ((GenericContainer) parent.asContainer()).nodes().add(child);
        return this;
    }

    protected PostOrderTreeBuilder<T> subtree(LinkedNode child) {
        if (child.isTree()) {
            var subtree = trees.pop();
            trees.peek().subtrees().add(subtree);
        }
        return this;
    }

    public LinkedTree tree() {
        if (trees.size() > 1) {
            throw new IllegalStateException();
        }
        return trees.isEmpty()
                ? null
                : trees.peek();
    }

    public GenericContainer mutableContainer(LinkedContainer.Type type, int nodes) {
        return new GenericContainer(
                type,
                nodes <= 0
                        ? Collections.emptyList()
                        : new ArrayList<>(nodes),
                root(),
                new HashMap<>());
    }

    protected LinkedTree root() {
        return trees.isEmpty()
                ? null
                : trees.peek();
    }

    protected Link cloneLink(String uri) {
        if (uri == null) {
            return null;
        }

        if (trees.isEmpty()) {
            return MutableLink.of(uri);
        }

        var root = (GenericTree) root();

        var link = root.linkMap().get(uri);

        if (link == null) {
            link = MutableLink.of(uri);
            root.linkMap().put(uri, link);
        }
        return link;
    }

}
