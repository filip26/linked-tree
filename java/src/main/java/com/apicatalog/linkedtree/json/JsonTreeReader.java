package com.apicatalog.linkedtree.json;

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
import com.apicatalog.linkedtree.traversal.NodeSelector;

import jakarta.json.JsonStructure;
import jakarta.json.JsonValue;

public abstract class JsonTreeReader implements NodeConsumer<JsonValue>, NodeSelector<JsonValue> {

    protected FragmentAdapterResolver fragmentAdapterResolver;
    protected Stack<Map<String, LinkedLiteralReader>> literalAdapters;

    protected Stack<LinkedNode> nodeStack;

    protected Stack<LinkedTree> trees;

    protected NodeSelector<JsonValue> nodeSelector;

    protected JsonTreeReader(
            FragmentAdapterResolver fragmentAdapterResolver,
            Map<String, LinkedLiteralReader> literalAdapters) {
        this.fragmentAdapterResolver = fragmentAdapterResolver;
        this.literalAdapters = new Stack<>();
        this.literalAdapters.push(literalAdapters);
        this.nodeStack = null;
        this.trees = null;
        this.nodeSelector = null;

    }

    public LinkedTree read(JsonStructure source, NodeSelector<JsonValue> selector) {
        nodeStack = new Stack<>();
        trees = new Stack<>();

        nodeSelector = selector;

        JsonTreeSearch.postOrder(this, source, this);

        return trees.peek();
    }

    @Override
    public ProcessingPolicy test(
            JsonValue node,
            int indexOrder,
            String indexTerm,
            int depth) {
        var policy = nodeSelector.test(node, indexOrder, indexTerm, depth);

        switch (policy) {
        case Accept, Stop -> nodeStack.push(clone(node));
        case Drop, Ignore -> {
        }
        default -> throw new IllegalArgumentException("Unexpected value: " + policy);
        }

        return policy;
    }

    @Override
    public void accept(JsonValue node, int indexOrder, String indexTerm, int depth) {

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

        if (child.isTree()) {
            var subtree = trees.pop();
            trees.peek().subtrees().add(subtree);
        }
    }

    protected abstract LinkedNode clone(JsonValue source);

    protected GenericContainer mutableContainer(LinkedContainer.Type type, int nodes) {
        return new GenericContainer(
                type,
                nodes <= 0
                        ? Collections.emptyList()
                        : new ArrayList<>(nodes),
                root(),
                new HashMap<>());
    }

    protected void pi(Collection<ProcessingInstruction> ops) {
        if (nodeStack.peek() instanceof GenericTree tree) {
            tree.ops().put(tree.nodes().size() + 1, ops);

        } else if (nodeStack.peek() instanceof GenericContainer container) {
            container.ops().put(container.nodes().size() + 1, ops);
        }
    }

    protected GenericFragment mutableFragment(
            String id,
            Collection<String> type,
            int properties,
            Collection<ProcessingInstruction> ops) {

        pi(ops);

        return new GenericFragment(
                cloneLink(id),

                type.isEmpty()
                        ? Collections.emptySet()
                        : Collections.unmodifiableCollection(type),

                properties <= 0
                        ? Collections.emptyMap()
                        : new LinkedHashMap<>(properties),

                root());

    }

    protected ImmutableLangString immutableLangString(
            String value,
            String language,
            LanguageDirectionType direction,
            Collection<ProcessingInstruction> ops) {
        pi(ops);
        return new ImmutableLangString(value, language, direction, root());
    }

    protected ImmutableLiteral immutableLiteral(
            String value,
            String datatype,
            Collection<ProcessingInstruction> ops) {
        pi(ops);
        return new ImmutableLiteral(value, datatype, root());
    }

    protected GenericTree mutableTree(
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

        var tree = new GenericTree(
                cloneLink(id),
                type.isEmpty()
                        ? Collections.emptySet()
                        : Collections.unmodifiableCollection(type),

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
