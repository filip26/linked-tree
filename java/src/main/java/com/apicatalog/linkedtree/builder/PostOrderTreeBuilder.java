package com.apicatalog.linkedtree.builder;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Stack;
import java.util.stream.Collectors;

import com.apicatalog.linkedtree.Linkable;
import com.apicatalog.linkedtree.LinkedContainer;
import com.apicatalog.linkedtree.LinkedContainer.ContainerType;
import com.apicatalog.linkedtree.LinkedFragment;
import com.apicatalog.linkedtree.LinkedLiteral;
import com.apicatalog.linkedtree.LinkedNode;
import com.apicatalog.linkedtree.LinkedTree;
import com.apicatalog.linkedtree.LinkedTreeError;
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
import com.apicatalog.linkedtree.reader.LinkedFragmentReader;
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
    public void accept(T node, int indexOrder, String indexTerm, int depth) throws LinkedTreeError {
        if (indexOrder != -1) {
            bind(indexOrder);

        } else if (indexTerm != null) {
            bind(indexTerm);

        } else {
            links(trees.peek().links());
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

    public PostOrderTreeBuilder<T> bind(String term) throws LinkedTreeError {

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

    public PostOrderTreeBuilder<T> bind(int index) throws LinkedTreeError {

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

    protected PostOrderTreeBuilder<T> subtree(LinkedNode child) throws LinkedTreeError {
        if (child.isTree()) {
            var subtree = trees.pop();
            trees.peek().subtrees().add(subtree);
            links(subtree.links());
        }
        return this;
    }

    protected void links(Collection<Link> links) throws LinkedTreeError {
        // process links
        for (final Link link : links) {
            ((MutableLink) link).target(
                    adapt(
                            (MutableLink) link,
                            mergeTypes(link.refs()),
                            merge(link.refs())));
        }
    }

    protected static Collection<String> mergeTypes(Collection<LinkedFragment> fragments) {
        return fragments.stream()
                .map(LinkedFragment::type)
                .flatMap(Type::stream)
                .collect(Collectors.toSet());
    }

    protected static Map<String, LinkedContainer> merge(Collection<LinkedFragment> fragments) {

        final Map<String, LinkedContainer> map = new HashMap<>(fragments.stream().map(LinkedFragment::terms)
                .mapToInt(Collection::size).sum());

        fragments.forEach(fragment -> toMap(fragment, map));

        return map;
    }

    protected static Map<String, LinkedContainer> toMap(LinkedFragment fragment, final Map<String, LinkedContainer> map) {
        if (fragment instanceof GenericFragment generic) {
            map.putAll(generic.entries());
        }
        for (final String term : fragment.terms()) {
            map.put(term, fragment.property(term));
        }
        return map;
    }

    protected LinkedFragment adapt(MutableLink id, Collection<String> type, Map<String, LinkedContainer> data) throws LinkedTreeError {

        var fragmentAdapter = fragmentAdapterResolver.resolve(
                id != null ? id.uri() : null,
                type);

        return materialize(
                fragmentAdapter != null
                        ? fragmentAdapter.reader()
                        : null,
                id,
                type,
                data);
    }

    protected LinkedFragment materialize(LinkedFragmentReader reader, MutableLink id, Collection<String> type, Map<String, LinkedContainer> data) throws LinkedTreeError {

        if (reader != null) {
            final Linkable fragment = reader.read(id, type, data);
            if (fragment != null) {
                return fragment.ld().asFragment();
            }
        }

        return mutableFragment(
                id,
                type,
                data,
                Collections.emptyList());
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

        var link = link(id);

        var fragment = mutableFragment(link, type,
                properties <= 0
                        ? Collections.emptyMap()
                        : new LinkedHashMap<>(properties),
                ops);

        if (link instanceof MutableLink mutableLink) {
            mutableLink.refs().add(fragment);
        }
        return fragment;

    }

    private GenericFragment mutableFragment(
            Link link,
            Collection<String> type,
            Map<String, LinkedContainer> properties,
            Collection<ProcessingInstruction> ops) {

        var types = type.isEmpty()
                ? Type.empty()
                : AdaptableType.of(type);

        pi(ops);

        var fragment = new GenericFragment(
                link,

                types,

                properties,

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
                ? Type.empty()
                : AdaptableType.of(type);

        var link = link(id);

        var tree = new GenericTree(
                link,

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
        if (link instanceof MutableLink mutableLink) {
            mutableLink.refs().add(tree);
        }

        trees.push(tree);
        return tree;
    }

    protected LinkedTree root() {
        return trees.isEmpty()
                ? null
                : trees.peek();
    }

    private Link link(String uri) {
        if (uri == null) {
            return null;
        }

        var root = (GenericTree) root();

        if (root == null) {
            return MutableLink.of(uri);
        }

        var link = root.linkMap().get(uri);

        if (link == null) {
            link = MutableLink.of(uri);
            root.linkMap().put(uri, link);
        }
        return link;
    }
}
