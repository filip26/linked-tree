package com.apicatalog.linkedtree.builder;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Stack;

import com.apicatalog.linkedtree.LinkedContainer;
import com.apicatalog.linkedtree.LinkedContainer.ContainerType;
import com.apicatalog.linkedtree.LinkedLiteral;
import com.apicatalog.linkedtree.LinkedNode;
import com.apicatalog.linkedtree.LinkedTree;
import com.apicatalog.linkedtree.adapter.resolver.FragmentAdapterResolver;
import com.apicatalog.linkedtree.lang.ImmutableLangString;
import com.apicatalog.linkedtree.lang.LangString.LanguageDirectionType;
import com.apicatalog.linkedtree.link.Link;
import com.apicatalog.linkedtree.link.MutableLink;
import com.apicatalog.linkedtree.literal.ImmutableLiteral;
import com.apicatalog.linkedtree.pi.ProcessingInstruction;
import com.apicatalog.linkedtree.primitive.GenericContainer;
import com.apicatalog.linkedtree.primitive.GenericFragment;
import com.apicatalog.linkedtree.primitive.GenericTree;
import com.apicatalog.linkedtree.reader.LinkedLiteralReader;
import com.apicatalog.linkedtree.traversal.NodeConsumer;
import com.apicatalog.linkedtree.type.AdaptableType;
import com.apicatalog.linkedtree.type.Type;

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
            int nodesCapacity,
            Collection<ProcessingInstruction> ops) {
        nodeStack.push(mutableTree(id, type, propertiesCapacity, nodesCapacity, ops));
        return this;
    }

    public PostOrderTreeBuilder<T> fragment(
            String id,
            Collection<String> type,
            int capacity,
            Collection<ProcessingInstruction> ops) {
        nodeStack.push(mutableFragment(
                id,
                type,
                capacity,
                ops));
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

        nodeStack.push(mutableContainer(ContainerType.UnorderedSet, capacity));
        return this;
    }

    public PostOrderTreeBuilder<T> list(int capacity) {
        nodeStack.push(mutableContainer(
                LinkedContainer.ContainerType.OrderedList,
                capacity));
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

        if (child.isContainer() && LinkedContainer.ContainerType.OrderedList == child.asContainer().containerType()) {
            nodeStack.pop();
            nodeStack.push(child);
            return this;

        }

        nodeStack.pop();
        parent = nodeStack.push(mutableContainer(ContainerType.UnorderedSet, 1));
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

    private GenericContainer mutableContainer(LinkedContainer.ContainerType type, int nodes) {
        return new GenericContainer(
                type,
                nodes <= 0
                        ? Collections.emptyList()
                        : new ArrayList<>(nodes),
                root(),
                new HashMap<>());
    }

    protected void pi(Collection<ProcessingInstruction> ops) {

        if (nodeStack.peek() == null) {
            nodeStack.pop();
            nodeStack.push(mutableContainer(ContainerType.UnorderedSet, 1));
        }

        if (nodeStack.peek() instanceof GenericTree tree) {
            tree.ops().put(tree.nodes().size() + 1, ops);

        } else if (nodeStack.peek() instanceof GenericContainer container) {
            container.ops().put(container.nodes().size() + 1, ops);
        }
    }

    private GenericFragment mutableFragment(
            String id,
            Collection<String> type,
            int properties,
            Collection<ProcessingInstruction> ops) {

        var types = type.isEmpty()
                ? Type.EMPTY
                : AdaptableType.of(type);

        pi(ops);

        var fragment = new GenericFragment(
                cloneLink(id),

                types,

                properties <= 0
                        ? Collections.emptyMap()
                        : new LinkedHashMap<>(properties),

                root());

        if (types instanceof AdaptableType adaptableType) {
            adaptableType.node(fragment);
        }

        return fragment;

    }

    protected PostOrderTreeBuilder<T> literal(
            LinkedLiteral literal) {
        nodeStack.push(literal);
        return this;
    }

    protected PostOrderTreeBuilder<T> immutableLangString(
            String value,
            String language,
            LanguageDirectionType direction,
            Collection<ProcessingInstruction> ops) {
        pi(ops);
        nodeStack.push(new ImmutableLangString(value, language, direction, root()));
        return this;
    }

    protected PostOrderTreeBuilder<T> immutableLiteral(
            String value,
            String datatype,
            Collection<ProcessingInstruction> ops) {
        pi(ops);
        nodeStack.push(new ImmutableLiteral(value, datatype, root()));
        return this;
    }

    private GenericTree mutableTree(
            String id,
            Collection<String> type,
            int properties,
            int nodes,
            Collection<ProcessingInstruction> ops) {

        var treeOps = new HashMap<Integer, Collection<ProcessingInstruction>>();

        if (!ops.isEmpty()) {
            treeOps.put(0, ops);
        }

        // clone links
//        final Map<String, Link> links = source.asTree().links().isEmpty()
//                ? Collections.emptyMap()
//                : source.asTree().links()
//                        .stream()
//                        .map(Link::uri)
//                        .collect(Collectors.toMap(
//                                Function.identity(),
//                                MutableLink::of));

        var types = type.isEmpty()
                ? Type.EMPTY
                : AdaptableType.of(type);

        var tree = new GenericTree(
                cloneLink(id),

                types,

                properties <= 0
                        ? Collections.emptyMap()
                        : new LinkedHashMap<>(properties),

                nodes <= 0
                        ? Collections.emptyList()
                        : new ArrayList<>(nodes),

                // links
                new HashMap<>(),
                // subtrees
                new ArrayList<>(),
                root(),
                treeOps);

        if (types instanceof AdaptableType adaptableType) {
            adaptableType.node(tree);
        }
        
        trees.push(tree);
        return tree;
    }

//        // initialize root
//        if (trees.isEmpty()) {
////            final GenericTree tree = treeInstance(switch (source.getValueType()) {
////            case OBJECT -> source.asJsonObject();
////            case ARRAY -> source.asJsonArray();
////            //TODO add a check, and if a root is not provided, create one, adaptive approach
////            default -> throw new IllegalArgumentException("An uknown root type, expected an object or an array, but got [" + source.getValueType() + "]");
////            });
////            
//        }
//        
//        switch (source.getValueType()) {
//        case OBJECT -> clone(source.asJsonObject());
//        case ARRAY -> clone(source.asJsonArray());
//        
//        }
//        
//        
//
//        return null;
//    }

    protected LinkedTree root() {
        return trees.isEmpty()
                ? null
                : trees.peek();
    }

    private Link cloneLink(String uri) {
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
