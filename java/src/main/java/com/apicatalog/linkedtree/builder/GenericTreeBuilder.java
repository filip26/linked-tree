package com.apicatalog.linkedtree.builder;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Stack;
import java.util.function.Function;
import java.util.stream.Collectors;

import com.apicatalog.linkedtree.LinkedContainer;
import com.apicatalog.linkedtree.LinkedNode;
import com.apicatalog.linkedtree.LinkedTree;
import com.apicatalog.linkedtree.lang.ImmutableLangString;
import com.apicatalog.linkedtree.lang.LangString;
import com.apicatalog.linkedtree.link.Link;
import com.apicatalog.linkedtree.link.MutableLink;
import com.apicatalog.linkedtree.literal.ImmutableLiteral;
import com.apicatalog.linkedtree.pi.ProcessingInstruction;
import com.apicatalog.linkedtree.primitive.GenericContainer;
import com.apicatalog.linkedtree.primitive.GenericFragment;
import com.apicatalog.linkedtree.primitive.GenericTree;
import com.apicatalog.linkedtree.traversal.DepthFirstSearch;
import com.apicatalog.linkedtree.traversal.NodeConsumer;
import com.apicatalog.linkedtree.traversal.NodeSelector;

public class GenericTreeBuilder implements NodeConsumer<LinkedNode>, NodeSelector<LinkedNode> {

    protected final LinkedTree sourceTree;

    protected Stack<LinkedNode> nodeStack;

    protected Stack<GenericTree> clonedTrees;

    protected NodeSelector<LinkedNode> nodeSelector;

    public GenericTreeBuilder(LinkedTree source) {
        this.sourceTree = source;
        this.nodeStack = null;
        this.clonedTrees = null;
        this.nodeSelector = null;
    }

    public LinkedTree deepClone(NodeSelector<LinkedNode> selector) {
        nodeStack = new Stack<>();
        clonedTrees = new Stack<>();

        nodeSelector = selector;

        DepthFirstSearch.postOrder(this, sourceTree, this);

        return clonedTrees.peek();
    }

    @Override
    public ProcessingPolicy test(LinkedNode node, int indexOrder, String indexTerm, int depth) {

        var policy = nodeSelector.test(node, indexOrder, indexTerm, depth);

        switch (policy) {
        case Accept, Stop -> nodeStack.push(clone(node));
        case Drop -> {
        }
        default -> throw new IllegalArgumentException("Unexpected value: " + policy);
        }

        return policy;
    }

    @Override
    public void accept(LinkedNode source, int indexOrder, String indexTerm, int depth) {

        final LinkedNode node = nodeStack.pop();

        if (nodeStack.isEmpty()) {
            return;
        }

        final LinkedNode parent = nodeStack.peek();

        if (indexOrder != -1) {
            if (parent.isTree()) {
                ((GenericTree) parent.asContainer()).nodes().add(node);

            } else if (parent.isContainer()) {
                ((GenericContainer) parent.asContainer()).nodes().add(node);

            } else {
                throw new IllegalStateException();
            }

        } else if (indexTerm != null) {

            if (parent.isTree()) {
                ((GenericTree) parent.asTree()).entries().put(indexTerm, node.asContainer());

            } else if (parent.isFragment()) {
                ((GenericFragment) parent.asFragment()).entries().put(indexTerm, node.asContainer());

            } else {
                throw new IllegalStateException();
            }
        }

        if (node.isTree()) {
            var subtree = clonedTrees.pop();
            clonedTrees.peek().subtrees().add(subtree);
        }
    }

    protected LinkedNode clone(LinkedNode source) {

        final LinkedTree root = clonedTrees.isEmpty()
                ? null
                : clonedTrees.peek();

        if (source.isTree()) {

            // clone links
            final Map<String, Link> links = source.asTree().links().isEmpty()
                    ? Collections.emptyMap()
                    : source.asTree().links()
                            .stream()
                            .map(Link::uri)
                            .collect(Collectors.toMap(
                                    Function.identity(),
                                    MutableLink::of));

            var tree = new GenericTree(
                    cloneLink(source.asTree().id()),

                    source.asTree().type().isEmpty()
                            ? Collections.emptySet()
                            : Collections.unmodifiableCollection(source.asTree().type()),

                    source.asFragment().terms().isEmpty()
                            ? Collections.emptyMap()
                            : new LinkedHashMap<>(source.asFragment().terms().size()),

                    source.asContainer().nodes().isEmpty()
                            ? Collections.emptyList()
                            : new ArrayList<>(source.asContainer().size()),

                    links,

                    source.asTree().subtrees().isEmpty()
                            ? Collections.emptyList()
                            : new ArrayList<>(source.asTree().subtrees().size()),

                    root,
                    cloneOps(source.asContainer()));

            clonedTrees.push(tree);

            return tree;

        } else if (source.isContainer()) {
            return new GenericContainer(
                    source.asContainer().containerType(),
                    source.asContainer().nodes().isEmpty()
                            ? Collections.emptyList()
                            : new ArrayList<>(source.asContainer().size()),
                    root,
                    cloneOps(source.asContainer()));

        } else if (source.isFragment()) {
            return new GenericFragment(
                    cloneLink(source.asFragment().id()),

                    source.asFragment().type().isEmpty()
                            ? Collections.emptySet()
                            : Collections.unmodifiableCollection(source.asFragment().type()),

                    source.asFragment().terms().isEmpty()
                            ? Collections.emptyMap()
                            : new LinkedHashMap<>(source.asFragment().terms().size()),

                    root);

        } else if (source.isLiteral()) {
            if (source.asLiteral() instanceof LangString langString) {
                return new ImmutableLangString(
                        langString.lexicalValue(),
                        langString.language(),
                        langString.direction(),
                        root);
            }
            return new ImmutableLiteral(
                    source.asLiteral().lexicalValue(),
                    source.asLiteral().datatype(),
                    root);
        }
        throw new IllegalStateException();
    }

    protected Link cloneLink(Link source) {
        if (source == null || source.uri() == null) {
            return null;
        }

        if (clonedTrees.isEmpty()) {
            return MutableLink.of(source.uri());
        }

        return ((GenericTree) clonedTrees.peek()).linkMap().get(source.uri());
    }

    protected Map<Integer, Collection<ProcessingInstruction>> cloneOps(LinkedContainer source) {
        if (source == null || source.size() == 0) {
            return Collections.emptyMap();
        }

        Map<Integer, Collection<ProcessingInstruction>> ops = new HashMap<>();

        for (int i = 0; i < source.size(); i++) {
            var pi = source.pi(i);
            if (pi != null) {
                ops.put(i, pi);
            }
        }

        return ops;
    }
}
