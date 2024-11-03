package com.apicatalog.linkedtree.builder;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Stack;

import com.apicatalog.linkedtree.LinkedContainer;
import com.apicatalog.linkedtree.LinkedNode;
import com.apicatalog.linkedtree.LinkedTree;
import com.apicatalog.linkedtree.fragment.GenericFragment;
import com.apicatalog.linkedtree.link.Link;
import com.apicatalog.linkedtree.link.MutableLink;
import com.apicatalog.linkedtree.pi.ProcessingInstruction;
import com.apicatalog.linkedtree.primitive.GenericContainer;
import com.apicatalog.linkedtree.primitive.GenericTree;
import com.apicatalog.linkedtree.traversal.NodeConsumer;
import com.apicatalog.linkedtree.traversal.NodeSelector;
import com.apicatalog.linkedtree.type.AdaptableType;
import com.apicatalog.linkedtree.type.FragmentType;

public class GenericTreeCompiler implements NodeConsumer<LinkedNode>, NodeSelector<LinkedNode> {

    protected Stack<LinkedNode> nodeStack;

    protected Stack<GenericTree> clonedTrees;

    protected NodeSelector<LinkedNode> nodeSelector;

    public GenericTreeCompiler() {
        this.nodeStack = null;
        this.clonedTrees = null;
        this.nodeSelector = null;

        nodeStack = new Stack<>();
        clonedTrees = new Stack<>();
        nodeSelector = (node, indexOrder, indexTerm, depth) -> TraversalPolicy.Accept;
    }

    @Override
    public TraversalPolicy test(LinkedNode node, int indexOrder, String indexTerm, int depth) throws TreeBuilderError {

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

            var tree = GenericTree.of(
                    source.asTree(),
                    cloneLink(source.asTree().id()),
                    root);

            clonedTrees.push(tree);

            return tree;

        } else if (source.isContainer()) {
            return new GenericContainer(
                    source.asContainer().containerType(),
                    new ArrayList<>(source.asContainer().size()),
                    root,
                    cloneOps(source.asContainer()));

        } else if (source.isFragment()) {

            var types = source.asFragment().type().isEmpty()
                    ? FragmentType.empty()
                    : AdaptableType.of(source.asFragment().type().stream().toList());

            var fragment = new GenericFragment(
                    cloneLink(source.asFragment().id()),

                    types,

                    new LinkedHashMap<>(source.asFragment().terms().size()),
                    root);

            if (types instanceof AdaptableType adaptableType) {
                adaptableType.node(fragment);
            }

            return fragment;

        } else if (source.isLiteral()) {
            // literals are immutable
            return source;
//            if (source.asLiteral() instanceof LangString langString) {
//                return new ImmutableLangString(
//                        langString.lexicalValue(),
//                        langString.language(),
//                        langString.direction(),
//                        root);
//            }
//            return new ImmutableLiteral(
//                    source.asLiteral().lexicalValue(),
//                    source.asLiteral().datatype());
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

    public static Map<Integer, Collection<ProcessingInstruction>> cloneOps(LinkedContainer source) {
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

    public LinkedTree tree() {
        return clonedTrees.peek();
    }
}
