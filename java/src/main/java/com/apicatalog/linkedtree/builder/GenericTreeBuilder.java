package com.apicatalog.linkedtree.builder;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Stack;

import com.apicatalog.linkedtree.Link;
import com.apicatalog.linkedtree.LinkedNode;
import com.apicatalog.linkedtree.LinkedTree;
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

    protected LinkedTree clonedTree;

    protected Map<String, Link> links;

    protected Stack<LinkedNode> nodeStack;

    protected NodeSelector nodeSelector;

    public GenericTreeBuilder(LinkedTree source) {
        this.sourceTree = source;
        this.links = Collections.emptyMap();
        this.nodeStack = new Stack<>();
    }

    public LinkedTree deepClone(NodeSelector selector) {
        this.links = new HashMap<>();
        this.nodeSelector = selector;
        DepthFirstSearch.postOrder(this, sourceTree, this);
        return clonedTree;
    }

    @Override
    public ProcessingPolicy test(LinkedNode node, int indexOrder, String indexTerm, int depth) {
        if (ProcessingPolicy.Accepted == nodeSelector.test(node, indexOrder, indexTerm, depth)) {
            nodeStack.push(clone(node));
            return ProcessingPolicy.Accepted;
        }
        return ProcessingPolicy.Dropped;
    }

    @Override
    public void accept(LinkedNode node, int indexOrder, String indexTerm, int depth) {

        final LinkedNode child = nodeStack.pop();

        if (nodeStack.isEmpty()) {
            clonedTree = child.asTree();
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
            ((GenericFragment) parent.asFragment()).entries().put(indexTerm, child.asContainer());
        }
    }

    protected LinkedNode clone(LinkedNode source) {
        if (source.isTree()) {
            return new GenericTree(
                    cloneLink(source.asTree().id()),
                    Collections.unmodifiableCollection(source.asTree().type()),
                    new LinkedHashMap<>(source.asFragment().terms().size()),
                    new ArrayList<>(source.asContainer().size()),
                    links,
                    null,
                    null,
                    null);

        } else if (source.isContainer()) {
            return new GenericContainer(
                    source.asContainer().containerType(),
                    new ArrayList<>(source.asContainer().size()),
                    null,
                    null);

        } else if (source.isFragment()) {
            return new GenericFragment(
                    cloneLink(source.asFragment().id()),
                    Collections.unmodifiableCollection(source.asFragment().type()),
                    new LinkedHashMap<>(source.asFragment().terms().size()),
                    null);

        } else if (source.isLiteral()) {
            // FIXME
            return new GenericLiteral(
                    source.asLiteral().lexicalValue(),
                    source.asLiteral().datatype());
        }
        throw new IllegalStateException();
    }

    protected MutableLink cloneLink(Link source) {
        if (source == null || source.uri() == null) {
            return null;
        }

        MutableLink link = (MutableLink) links.get(source.uri());
        if (link == null) {
            link = MutableLink.of(source.uri());
            links.put(source.uri(), link);
        }

        return link;
    }
}
