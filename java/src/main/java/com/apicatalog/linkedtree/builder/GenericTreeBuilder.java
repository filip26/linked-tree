package com.apicatalog.linkedtree.builder;

import java.util.ArrayList;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Stack;
import java.util.function.Function;
import java.util.function.Supplier;
import java.util.stream.Collectors;

import com.apicatalog.linkedtree.Link;
import com.apicatalog.linkedtree.LinkedNode;
import com.apicatalog.linkedtree.LinkedTree;
import com.apicatalog.linkedtree.lang.ImmutableLangString;
import com.apicatalog.linkedtree.lang.LangString;
import com.apicatalog.linkedtree.primitive.GenericContainer;
import com.apicatalog.linkedtree.primitive.GenericFragment;
import com.apicatalog.linkedtree.primitive.GenericLiteral;
import com.apicatalog.linkedtree.primitive.GenericTree;
import com.apicatalog.linkedtree.primitive.MutableLink;
import com.apicatalog.linkedtree.traversal.DepthFirstSearch;
import com.apicatalog.linkedtree.traversal.NodeConsumer;
import com.apicatalog.linkedtree.traversal.NodeSelector;

public class GenericTreeBuilder implements NodeConsumer, NodeSelector {

    protected final LinkedTree sourceTree;

    protected Stack<LinkedNode> nodeStack;

    protected Stack<GenericTree> clonedTrees;

    protected NodeSelector nodeSelector;

    public GenericTreeBuilder(LinkedTree source) {
        this.sourceTree = source;
        this.nodeStack = null;
        this.clonedTrees = null;
        this.nodeSelector = null;
    }

    public LinkedTree deepClone(NodeSelector selector) {
        nodeStack = new Stack<>();
        clonedTrees = new Stack<>();

        nodeSelector = selector;

        DepthFirstSearch.postOrder(this, sourceTree, this);

        return clonedTrees.peek();
    }

    @Override
    public ProcessingPolicy test(LinkedNode node, int indexOrder, String indexTerm, int depth) {
        if (ProcessingPolicy.Accepted == nodeSelector.test(node, indexOrder, indexTerm, depth)) {
            nodeStack.push(clone(
                    node,
                    clonedTrees.isEmpty()
                            ? () -> null
                            : () -> clonedTrees.peek()));
            return ProcessingPolicy.Accepted;
        }
        return ProcessingPolicy.Dropped;
    }

    @Override
    public void accept(LinkedNode node, int indexOrder, String indexTerm, int depth) {

        final LinkedNode child = nodeStack.pop();

        if (nodeStack.isEmpty()) {
            return;
        }

        final LinkedNode parent = nodeStack.peek();

        if (indexOrder != -1) {
            if (parent.isTree()) {
                ((GenericTree) parent.asContainer()).nodes().add(child);

            } else if (parent.isContainer()) {
                ((GenericContainer) parent.asContainer()).nodes().add(child);

            } else {
                throw new IllegalStateException();
            }

        } else if (indexTerm != null) {

            if (parent.isTree()) {
                ((GenericTree) parent.asTree()).entries().put(indexTerm, child.asContainer());

            } else if (parent.isFragment()) {
                ((GenericFragment) parent.asFragment()).entries().put(indexTerm, child.asContainer());

            } else {
                throw new IllegalStateException();
            }
        }

        if (node.isTree()) {
            var subtree = clonedTrees.pop();
            clonedTrees.peek().subtrees().add(subtree);
        }
    }

    protected LinkedNode clone(LinkedNode source, Supplier<LinkedTree> treeSupplier) {
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

                    treeSupplier,
                    Collections.emptyMap());

            clonedTrees.push(tree);

            return tree;

        } else if (source.isContainer()) {
            return new GenericContainer(
                    source.asContainer().containerType(),
                    source.asContainer().nodes().isEmpty()
                            ? Collections.emptyList()
                            : new ArrayList<>(source.asContainer().size()),
                    treeSupplier,
                    () -> Collections.emptyMap());

        } else if (source.isFragment()) {
            return new GenericFragment(
                    cloneLink(source.asFragment().id()),

                    source.asFragment().type().isEmpty()
                            ? Collections.emptySet()
                            : Collections.unmodifiableCollection(source.asFragment().type()),

                    source.asFragment().terms().isEmpty()
                            ? Collections.emptyMap()
                            : new LinkedHashMap<>(source.asFragment().terms().size()),

                    treeSupplier);

        } else if (source.isLiteral()) {
            if (source.asLiteral() instanceof LangString langString) {
                return new ImmutableLangString(
                        langString.lexicalValue(),
                        langString.language(),
                        langString.direction(),
                        treeSupplier);
            }
            return new GenericLiteral(
                    source.asLiteral().lexicalValue(),
                    source.asLiteral().datatype(),
                    treeSupplier);
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

//        var links = ((GenericTree)clonedTrees.peek()).linkMap();
//        
//        Link link = links.get(source.uri());
//        if (link == null) {
//            link = MutableLink.of(source.uri());
//            links.put(source.uri(), link);
//        }
//
//        return link;
    }
}
