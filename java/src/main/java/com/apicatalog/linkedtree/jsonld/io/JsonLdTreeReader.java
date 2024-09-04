package com.apicatalog.linkedtree.jsonld.io;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Stack;
import java.util.stream.Collectors;

import com.apicatalog.linkedtree.LinkedContainer;
import com.apicatalog.linkedtree.LinkedFragment;
import com.apicatalog.linkedtree.LinkedLiteral;
import com.apicatalog.linkedtree.LinkedNode;
import com.apicatalog.linkedtree.LinkedTree;
import com.apicatalog.linkedtree.adapter.LinkedFragmentAdapter;
import com.apicatalog.linkedtree.adapter.LinkedLiteralAdapter;
import com.apicatalog.linkedtree.adapter.resolver.FragmentAdapterResolver;
import com.apicatalog.linkedtree.adapter.resolver.TypeMapAdapterResolver;
import com.apicatalog.linkedtree.builder.TreeBuilderContext;
import com.apicatalog.linkedtree.json.JsonDecimal;
import com.apicatalog.linkedtree.json.JsonInteger;
import com.apicatalog.linkedtree.json.JsonLiteral;
import com.apicatalog.linkedtree.json.JsonScalar;
import com.apicatalog.linkedtree.json.JsonTreeReader;
import com.apicatalog.linkedtree.json.pi.JsonObjectWrite;
import com.apicatalog.linkedtree.jsonld.JsonLdId;
import com.apicatalog.linkedtree.jsonld.JsonLdKeyword;
import com.apicatalog.linkedtree.jsonld.JsonLdType;
import com.apicatalog.linkedtree.lang.ImmutableLangString;
import com.apicatalog.linkedtree.link.Link;
import com.apicatalog.linkedtree.link.MutableLink;
import com.apicatalog.linkedtree.literal.ImmutableLiteral;
import com.apicatalog.linkedtree.pi.ProcessingInstruction;
import com.apicatalog.linkedtree.primitive.GenericFragment;
import com.apicatalog.linkedtree.reader.LinkedFragmentReader;
import com.apicatalog.linkedtree.reader.LinkedLiteralReader;
import com.apicatalog.linkedtree.traversal.NodeSelector;
import com.apicatalog.linkedtree.xsd.XsdConstants;

import jakarta.json.JsonArray;
import jakarta.json.JsonNumber;
import jakarta.json.JsonObject;
import jakarta.json.JsonString;
import jakarta.json.JsonStructure;
import jakarta.json.JsonValue;
import jakarta.json.JsonValue.ValueType;

public class JsonLdTreeReader extends JsonTreeReader {

    protected FragmentAdapterResolver fragmentAdapterResolver;
    protected Stack<Map<String, LinkedLiteralReader>> literalAdapters;

    protected JsonLdTreeReader(
            FragmentAdapterResolver fragmentAdapterResolver,
            Map<String, LinkedLiteralReader> literalAdapters) {
        super(fragmentAdapterResolver, literalAdapters);
    }

//    public static JsonLdTreeReader with(LinkedLiteralAdapter... literalAdapters) {
//        return with(null, literalAdapters);
//    }

//    public static JsonLdTreeReader with(FragmentAdapterResolver fragmentAdapterResolver, LinkedLiteralAdapter... literalAdapters) {
//        return new JsonLdTreeReader(fragmentAdapterResolver, literalAdapters != null
//                ? Arrays.stream(literalAdapters)
//                        .collect(
//                                Collectors.toMap(
//                                        LinkedLiteralAdapter::datatype,
//                                        Function.identity()))
//                : Collections.emptyMap());
//    }

    public LinkedTree read(JsonStructure source) {
        return read(Collections.emptyList(), source);
    }

    public LinkedTree read(List<String> context, JsonStructure source) {
        // TODO context
        return read(source, ((node, indexOrder, indexTerm, depth) -> ProcessingPolicy.Accept));
    }

    @Override
    public LinkedTree read(JsonStructure source, NodeSelector<JsonValue> selector) {
        return super.read(source, (node, indexOrder, indexTerm, depth) -> {

            if (JsonLdKeyword.ID.equals(indexTerm)
                    || JsonLdKeyword.TYPE.equals(indexTerm)) {
                return ProcessingPolicy.Drop;
            }

            // do not follow @value objects
            if (ValueType.OBJECT == node.getValueType()) {

                if (node.asJsonObject().containsKey(JsonLdKeyword.VALUE)) {
                    return ProcessingPolicy.Stop;
                }

            }

            // merge root tree and following container
//            if (trees.size() == 1 
//                    && nodeStack.size() == 1
//                    && 
//                    ) {

//            }

            return selector.test(node, indexOrder, indexTerm, depth);
        });

    }

    @Override
    protected LinkedNode clone(final JsonValue source) {

        if (ValueType.OBJECT == source.getValueType()) {
            // tree
            if (source.asJsonObject().containsKey(JsonLdKeyword.GRAPH)) {

                return cloneTree(source.asJsonObject());

                // literal
            } else if (source.asJsonObject().containsKey(JsonLdKeyword.VALUE)) {
                // FIXME ops
                return readLiteral(source.asJsonObject(), new ArrayList<>());

                // list
            } else if (source.asJsonObject().containsKey(JsonLdKeyword.LIST)) {

            }
            // fragment
            return cloneFragment(source.asJsonObject());
        }

        // container or root tree
        if (ValueType.ARRAY == source.getValueType()) {
            if (trees.isEmpty()) {
                return cloneRoot(source.asJsonArray());
            }
            return cloneContainer(source.asJsonArray(), LinkedContainer.Type.UnorderedSet);
        }

        throw new IllegalArgumentException("The input is not an expanded JSON-LD, expected JSON object or an array but got " + source);
    }

    protected LinkedNode cloneTree(final JsonObject source) {
        return mutableTree(
                JsonLdId.string(source),
                JsonLdType.strings(source),
                source.size(),
                source.getJsonArray(JsonLdKeyword.GRAPH).size());
    }

    protected LinkedNode cloneRoot(final JsonArray source) {
        return mutableTree(
                null,
                Collections.emptySet(),
                0,
                source.size());
    }

    protected LinkedNode cloneFragment(final JsonObject source) {
        return mutableFragment(
                JsonLdId.string(source),
                JsonLdType.strings(source),
                source.size());
    }

    protected LinkedNode cloneContainer(final JsonArray source, LinkedContainer.Type type) {
        return mutableContainer(type, source.size());
    }

//    public LinkedTree readExpanded(JsonArray jsonNodes, TreeBuilderContext ctx) throws LinkedReaderError {
//        return readExpanded(null, jsonNodes, ctx);
//    }

//    public LinkedTree readExpanded(Collection<String> context, JsonArray jsonNodes, TreeBuilderContext ctx) throws LinkedReaderError {
//
//        if (jsonNodes.isEmpty()) {
//            return LinkedTree.EMPTY;
//        }
//
//        final Map<String, Link> links = new HashMap<>();
//
//        final Map<Integer, Collection<ProcessingInstruction>> opsMap = new HashMap<>();
//
//        final Collection<LinkedTree> subtrees = new ArrayList<>(2);
//
//        final LinkableInjector<LinkedTree> treeInjector = new LinkableInjector<>();
//
//        final LinkedTree tree = GenericTree.of(readNodes(jsonNodes, links, subtrees, ctx, opsMap), links, subtrees, () -> null, opsMap);
//
//        for (final Link link : links.values()) {
//            ((MutableLink) link).target(adapt(((MutableLink) link), mergeTypes(link.refs()), merge(link.refs()), ctx));
//        }
//
//        if (context != null && !context.isEmpty()) {
//            opsMap.put(0, List.of(new JsonLdContext(context)));
//        }
//
//        treeInjector.accept(tree);
//
//        return tree;
//    }
//
//    public LinkedTree readExpanded(JsonObject jsonNode, TreeBuilderContext ctx) throws LinkedReaderError {
//
//        final Map<String, Link> links = new HashMap<>();
//
//        final Map<Integer, Collection<ProcessingInstruction>> opsMap = new HashMap<>(1);
//        final Collection<ProcessingInstruction> ops = new ArrayList<>(2);
//
//        final Collection<LinkedTree> subtrees = new ArrayList<>(2);
//
//        final LinkableInjector<LinkedTree> treeInjector = new LinkableInjector<>();
//
//        final LinkedTree tree = GenericTree.of(readNode(jsonNode, links, subtrees, ctx, ops), links, subtrees, () -> null, opsMap);
//
//        if (!ops.isEmpty()) {
//            opsMap.put(0, ops);
//        }
//
//        for (final Link link : links.values()) {
//            ((MutableLink) link).target(adapt(((MutableLink) link), mergeTypes(link.refs()), merge(link.refs()), ctx));
//        }
//
//        treeInjector.accept(tree);
//
//        return tree;
//    }

    protected static Collection<String> mergeTypes(Collection<LinkedFragment> fragments) {
        return fragments.stream()
                .map(LinkedFragment::type)
                .flatMap(Collection::stream)
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

//    public Collection<LinkedNode> readNodes(final JsonArray jsonNodes, Map<String, Link> links, final Collection<LinkedTree> subtrees, final TreeBuilderContext ctx,
//            final Map<Integer, Collection<ProcessingInstruction>> nodeOps) throws LinkedReaderError {
//
//        if (jsonNodes.isEmpty()) {
//            return Collections.emptyList();
//        }
//
//        final Collection<LinkedNode> nodes = new ArrayList<>(jsonNodes.size());
//
//        Collection<ProcessingInstruction> ops = new ArrayList<>(2);
//
//        for (final JsonValue jsonValue : jsonNodes) {
//
//            if (jsonValue == null || !ValueType.OBJECT.equals(jsonValue.getValueType())) {
//                throw new IllegalArgumentException();
//            }
//            final LinkedNode node = readNode(jsonValue.asJsonObject(), links, subtrees, ctx, ops);
//
//            if (!ops.isEmpty()) {
//                // FIXME merge
//                nodeOps.put(nodes.size() + 1, ops);
//                ops = new ArrayList<>(2);
//            }
//
//            nodes.add(node);
//        }
//
//        return nodes;
//    }
//
//    protected LinkedContainer readValueArray(JsonArray values, Map<String, Link> links, Collection<LinkedTree> subtrees, TreeBuilderContext ctx) throws LinkedReaderError {
//
//        final Collection<LinkedNode> nodes = new ArrayList<>(values.size());
//
//        final Map<Integer, Collection<ProcessingInstruction>> nodeOps = new HashMap<>();
//
//        Collection<ProcessingInstruction> ops = new ArrayList<>(2);
//
//        for (final JsonValue item : values) {
//            final LinkedNode node = readValue(item, links, subtrees, ctx, ops);
//
//            if (!ops.isEmpty()) {
//                // FIXME merge
//                nodeOps.put(nodes.size() + 1, ops);
//                ops = new ArrayList<>(2);
//            }
//
//            nodes.add(node);
//        }
//
//        if (nodes.size() == 1 && nodes.iterator().next().isContainer()) {
//            return nodes.iterator().next().asContainer();
//            // FIXME node ops
//            // (nodes.iterator().next()).mergetOps(ops) or ops consumer?
//        }
//
//        return new GenericContainer(LinkedContainer.Type.UnorderedSet, nodes, ctx.rootSupplier(), () -> nodeOps);
//    }
//
//    protected LinkedNode readValue(JsonValue value, Map<String, Link> links, Collection<LinkedTree> subtrees, TreeBuilderContext ctx, Collection<ProcessingInstruction> ops) throws LinkedReaderError {
//
////      if (JsonUtils.isNotObject(value)) {
////      throw new DocumentError(ErrorType.Invalid, "Document");
////  }
//
//        final JsonObject object = value.asJsonObject();
//
//        return object.containsKey(JsonLdKeyword.VALUE)
//                ? readLiteral(object, ctx, ops)
//                : readNode(object, links, subtrees, ctx, ops);
//    }
//
//    protected LinkedNode readNode(JsonObject jsonObject, Map<String, Link> links, Collection<LinkedTree> subtrees, TreeBuilderContext ctx, Collection<ProcessingInstruction> ops)
//            throws LinkedReaderError {
//
//        if (jsonObject != null) {
//            if (jsonObject.containsKey(JsonLdKeyword.LIST)) {
//                return readList(jsonObject, links, subtrees, ctx);
//            }
//            if (jsonObject.containsKey(JsonLdKeyword.GRAPH)) {
//                return readGraph(jsonObject, subtrees, ctx);
//            }
//            if (jsonObject.size() == 1 && jsonObject.containsKey(JsonLdKeyword.ID)) {
//                final MutableLink link = getOrCreate(jsonObject.getString(JsonLdKeyword.ID), links);
//                final ImmutableReference ref = new ImmutableReference(link);
//                link.addFragment(ref);
//                return ref;
//            }
////      if (isContainer(jsonObject, Keywords.REVERSE)) {
////      return readReverse(jsonObject);
////  }        
//        }
//        return readFragment(jsonObject, links, subtrees, ctx, ops);
//    }
//
//    // list is a collection attribute
//    protected LinkedContainer readList(JsonObject jsonObject, Map<String, Link> links, Collection<LinkedTree> subtrees, TreeBuilderContext ctx) throws LinkedReaderError {
//
//        final JsonArray list = jsonObject.getJsonArray(JsonLdKeyword.LIST);
//
//        final Collection<LinkedNode> nodes = new ArrayList<>(list.size());
//
//        final Map<Integer, Collection<ProcessingInstruction>> nodeOps = new HashMap<>();
//
//        Collection<ProcessingInstruction> ops = new ArrayList<>(2);
//
//        for (final JsonValue item : list) {
//            final LinkedNode node = readValue(item, links, subtrees, ctx, ops);
//
//            if (!ops.isEmpty()) {
//                // FIXME merge
//                nodeOps.put(nodes.size() + 1, ops);
//                ops = new ArrayList<>(2);
//            }
//
//            nodes.add(node);
//        }
//
//        return new GenericContainer(LinkedContainer.Type.OrderedList, nodes, ctx.rootSupplier(), () -> nodeOps);
//    }
//
//    protected LinkedTree readGraph(JsonObject jsonObject, Collection<LinkedTree> subtrees, TreeBuilderContext ctx) throws LinkedReaderError {
//
//        final JsonArray graph = jsonObject.getJsonArray(JsonLdKeyword.GRAPH);
//
//        final Map<String, Link> links = new HashMap<>();
//
//        final Collection<LinkedTree> subsubtrees = new ArrayList<>();
//
//        String id = null;
//        Collection<String> types = Collections.emptySet();
//
//        final Map<String, LinkedContainer> properties = new HashMap<>(jsonObject.size() - 1);
//        final Map<String, JsonValue> unprocessed = new HashMap<>();
//
//        final LinkableInjector<LinkedTree> treeInjector = new LinkableInjector<>();
//
//        final Map<Integer, Collection<ProcessingInstruction>> nodeOps = new HashMap<>();
//
//        final Collection<LinkedNode> nodes = readNodes(graph, links, subsubtrees, ctx, nodeOps);
//
//        for (final Entry<String, JsonValue> entry : jsonObject.entrySet()) {
//
//            if (JsonLdKeyword.GRAPH.equals(entry.getKey())) {
//                continue;
//            }
//
//            if (JsonLdKeyword.ID.equals(entry.getKey())) {
//                if (ValueType.STRING.equals(entry.getValue().getValueType())) {
//                    id = ((JsonString) entry.getValue()).getString();
//                }
//
//            } else if (JsonLdKeyword.TYPE.equals(entry.getKey())) {
//
//                types = entry.getValue().asJsonArray().stream().map(JsonString.class::cast)
//                        .map(JsonString::getString)
//                        .toList();
//
//            } else if (entry.getKey().startsWith("@")) {
//                unprocessed.put(entry.getKey(), entry.getValue());
//
//            } else {
//                properties.put(entry.getKey(), readValueArray(entry.getValue().asJsonArray(), links, subsubtrees, ctx));
//            }
//        }
//
//        for (final Link link : links.values()) {
//            ((MutableLink) link).target(adapt(((MutableLink) link), mergeTypes(link.refs()), merge(link.refs()), ctx));
//        }
//
//        final GenericTree tree;
//
//        if (id != null) {
//            final MutableLink link = getOrCreate(id, links);
//            tree = new GenericTree(
//                    link,
//                    types,
//                    properties,
//                    nodes,
//                    links,
//                    subsubtrees,
//                    ctx.rootSupplier(),
//                    nodeOps);
//            link.addFragment(tree);
//
//        } else {
//            tree = new GenericTree(null, types, properties, nodes, links, subsubtrees, ctx.rootSupplier(), nodeOps);
//        }
//
//        treeInjector.accept(tree);
//        nodeOps.put(0, List.of(new JsonObjectWrite(unprocessed)));
//        subtrees.add(tree);
//        subtrees.addAll(subsubtrees);
//
//        return tree;
//    }
//
//    protected LinkedFragment readFragment(JsonObject value, Map<String, Link> links, Collection<LinkedTree> subtrees, TreeBuilderContext ctx, Collection<ProcessingInstruction> ops)
//            throws LinkedReaderError {
//
//        String id = null; // FIXME
//        final Collection<String> types = JsonLdType.strings(value);
//
//        final LinkedFragmentAdapter adapter = adapter(null, types, value);
//        if (adapter != null && adapter.literalAdapters() != null && !adapter.literalAdapters().isEmpty()) {
//            var adapters = new LinkedHashMap<>(literalAdapters.peek());
//            adapter.literalAdapters().forEach(a -> adapters.put(a.datatype(), a));
//            literalAdapters.push(adapters);
//        }
//
//        final Map<String, JsonValue> unprocessed = new HashMap<>();
//        final Map<String, LinkedContainer> properties = new HashMap<>(value.size());
//
//        for (final Entry<String, JsonValue> entry : value.entrySet()) {
//
//            // FIXME remove
//            if ("@id".equals(entry.getKey())) {
//                if (ValueType.STRING.equals(entry.getValue().getValueType())) {
//
//                    id = ((JsonString) entry.getValue()).getString();
//
////                    if (!idValue.startsWith("_:")) {
////                        id = idValue;
////                    }
//                }
//
//            } else if ("@type".equals(entry.getKey())) {
//                continue;
//
//            } else if (entry.getKey().startsWith("@")) {
//                unprocessed.put(entry.getKey(), entry.getValue());
//
//            } else {
//                properties.put(entry.getKey(), readValueArray(entry.getValue().asJsonArray(), links, subtrees, ctx));
//            }
//        }
//
//        if (adapter != null && adapter.literalAdapters() != null && !adapter.literalAdapters().isEmpty()) {
//            literalAdapters.pop();
//        }
//
//        if (id != null) {
//            final MutableLink link = getOrCreate(id, links);
//            final GenericFragment node = new GenericFragment(
//                    link,
//                    types,
//                    properties,
//                    ctx.rootSupplier());
//            if (!unprocessed.isEmpty()) {
//                ops.add(new JsonObjectWrite(unprocessed));
//            }
//            link.addFragment(node);
//            return node;
//        }
//
//        if (!unprocessed.isEmpty()) {
//            ops.add(new JsonObjectWrite(unprocessed));
//        }
//        return adapt(null, types, properties, ctx);
//    }

//    protected LinkedFragment adapt(MutableLink id, Collection<String> type, Map<String, LinkedContainer> data, TreeBuilderContext ctx) throws LinkedReaderError {
//
//        var fragmentAdapter = fragmentAdapterResolver.resolve(
//                id != null ? id.uri() : null,
//                type,
//                (t) -> null);
//
//        return materialize(
//                fragmentAdapter != null
//                        ? fragmentAdapter.reader()
//                        : null,
//                id,
//                type,
//                data,
//                ctx);
//    }

//    protected LinkedFragmentAdapter adapter(MutableLink id, Collection<String> type, JsonObject jsonObject) {
//
////      final StringValueSelector selector = (term) -> jsonObject.getString(term);
//
//        return fragmentAdapterResolver.resolve(
//                id != null ? id.uri() : null,
//                type,
//                (t) -> null);
//
//    }

//    protected LinkedFragment materialize(LinkedFragmentReader reader, MutableLink id, Collection<String> type, Map<String, LinkedContainer> data, TreeBuilderContext ctx) throws LinkedReaderError {
//
//        if (reader != null) {
//            final Linkable fragment = reader.read(id, type, data, ctx);
//            if (fragment != null) {
//                return fragment.ld().asFragment();
//            }
//        }
//
//        return new GenericFragment(
//                id,
//                type,
//                data,
//                ctx.rootSupplier());
//    }

    protected static MutableLink getOrCreate(String uri, Map<String, Link> links) {

        MutableLink link = (MutableLink) links.get(uri);
        if (link == null) {
            link = MutableLink.of(uri);
            links.put(uri, link);
        }

        return link;
    }

    protected LinkedLiteral readLiteral(final JsonObject valueJsonObject, final Collection<ProcessingInstruction> ops) {

//        final String value = getLiteralValue(valueObject);
//        final String datatype = getLiteralDataType(valueObject);
//        final String language = getLiteralLanguage(valueObject);
//
//        // for each adapter
//        // adapter.read(value) == null contine;
//
////        return JsonLdLiteral.of();
//        return GenericLinkedLiteral.of(value, datatype, language, null); // TODO
//    }
//
//    protected static String getLiteralValue(JsonObject item) {

        final JsonValue value = valueJsonObject.get(JsonLdKeyword.VALUE);

        String datatype = valueJsonObject.containsKey(JsonLdKeyword.TYPE)
                && ValueType.STRING.equals(valueJsonObject.get(JsonLdKeyword.TYPE).getValueType())
                        ? valueJsonObject.getString(JsonLdKeyword.TYPE)
                        : null;

        if (JsonLdKeyword.JSON.equals(datatype)) {
            var pi = getPi(valueJsonObject, JsonLdKeyword.VALUE, JsonLdKeyword.TYPE);
            if (pi != null) {
                ops.add(pi);
            }
            return JsonLiteral.of(value);

        } else if (value != null &&
                (ValueType.TRUE.equals(value.getValueType())
                        || ValueType.FALSE.equals(value.getValueType()))) {

            var pi = getPi(valueJsonObject, JsonLdKeyword.TYPE, JsonLdKeyword.VALUE);
            if (pi != null) {
                ops.add(pi);
            }

            return new JsonScalar(value,
                    datatype != null
                            ? datatype
                            : XsdConstants.BOOLEAN);

        } else if (value != null && ValueType.NUMBER.equals(value.getValueType())) {

            JsonNumber number = ((JsonNumber) value);

            if ((!number.isIntegral() && number.doubleValue() % -1 != 0)
                    || XsdConstants.DOUBLE.equals(datatype)
                    || XsdConstants.FLOAT.equals(datatype)
                    || number.bigDecimalValue().compareTo(BigDecimal.ONE.movePointRight(21)) >= 0) {

                var pi = getPi(valueJsonObject, JsonLdKeyword.TYPE, JsonLdKeyword.VALUE);
                if (pi != null) {
                    ops.add(pi);
                }
                return JsonDecimal.of(number,
                        datatype != null
                                ? datatype
                                : XsdConstants.DOUBLE);

            } else {
                var pi = getPi(valueJsonObject, JsonLdKeyword.TYPE, JsonLdKeyword.VALUE);
                if (pi != null) {
                    ops.add(pi);
                }

                return JsonInteger.of(
                        number,
                        datatype != null
                                ? datatype
                                : XsdConstants.INTEGER);
            }

        }

        if (value == null || !ValueType.STRING.equals(value.getValueType())) {
            return null;
        }

        if (datatype == null) {
            datatype = XsdConstants.STRING;
        }

        String valueString = ((JsonString) value).getString();

        if (literalAdapters != null && !literalAdapters.isEmpty()) {
            final LinkedLiteralReader adapter = literalAdapters.peek().get(datatype);
            if (adapter != null) {
                var pi = getPi(valueJsonObject, JsonLdKeyword.VALUE, JsonLdKeyword.TYPE);
                if (pi != null) {
                    ops.add(pi);
                }
                return adapter.read(valueString, root());
            }
        }

        if (XsdConstants.STRING.equals(datatype)) {
            // TODO direction
            var pi = getPi(valueJsonObject, JsonLdKeyword.VALUE, JsonLdKeyword.TYPE, JsonLdKeyword.LANGUAGE);
            if (pi != null) {
                ops.add(pi);
            }

            return new ImmutableLangString(
                    valueString,
                    getLiteralLanguage(valueJsonObject),
                    null, // FIXME direction
                    root());
        }

        var pi = getPi(valueJsonObject, JsonLdKeyword.VALUE, JsonLdKeyword.TYPE);
        if (pi != null) {
            ops.add(pi);
        }

        return new ImmutableLiteral(
                valueString,
                datatype,
                null);
    }

    protected static JsonObjectWrite getPi(JsonObject valueJsonObject, String... filter) {
        final Map<String, JsonValue> pi = new HashMap<>();

        for (final Map.Entry<String, JsonValue> jsonEntry : valueJsonObject.entrySet()) {
            if (JsonLdKeyword.anyMatch(jsonEntry.getKey(), filter)) {
                continue;
            }
            pi.put(jsonEntry.getKey(), jsonEntry.getValue());
        }

        if (pi.isEmpty()) {
            return null;
        }

        return new JsonObjectWrite(pi);
    }

    protected static String getLiteralDataType(JsonObject valueObject) {

        final JsonValue jsonType = valueObject.get("@type");
        if (jsonType == null || ValueType.NULL.equals(jsonType.getValueType())) {
            return null;
        }
        if (!ValueType.STRING.equals(jsonType.getValueType())) {
            throw new IllegalArgumentException();
        }

        return ((JsonString) jsonType).getString();
    }

    protected static String getLiteralLanguage(JsonObject valueObject) {

        final JsonValue jsonType = valueObject.get(JsonLdKeyword.LANGUAGE);
        if (jsonType == null || ValueType.NULL.equals(jsonType.getValueType())) {
            return null;
        }
        if (!ValueType.STRING.equals(jsonType.getValueType())) {
            throw new IllegalArgumentException();
        }

        return ((JsonString) jsonType).getString();
    }

//    protected LinkedFragment genericObject(JakartaNodeContext context, JsonObject value) {

//  for (final LinkedNodeAdapter<JsonObject, JsonValue, LinkedFragment> reader : nodeReaders) {
    // if (reader.accepts(null)) {
////      final LinkedFragment node = reader.read(context, link, null, value);
//////      link.target(node);
////      return node;
////  }
//  }
//  return GenericLinkedNode.of(null, null, null)
//
//        final String id = value.getString("@id");
////        final Link link = id != null ? context.link(id) : null;
//        // id -> link
//        // types
//
////        for (final LinkedNodeAdapter<JsonObject, JsonValue, LinkedFragment> reader : nodeReaders) {
////            if (reader.accepts(null)) {
////                final LinkedFragment node = reader.read(context, link, null, value);
//////                link.target(node);
////                return node;
////            }
////        }
//        throw new IllegalStateException();
//    }

    public static final Builder create() {
        return new Builder();
    }

    public static class Builder {

        protected TypeMapAdapterResolver.Builder fragmentMap;
        protected Map<String, LinkedLiteralReader> literalMap;

        public Builder() {
            this.fragmentMap = new TypeMapAdapterResolver.Builder();
            this.literalMap = new LinkedHashMap<>();
        }

        public Builder with(String type, LinkedFragmentReader reader) {
            fragmentMap.add(type, reader);
            return this;
        }

        public Builder with(String type, LinkedFragmentAdapter adapter) {
            fragmentMap.add(type, adapter);
            return this;
        }

        public Builder with(LinkedLiteralAdapter adapter) {
            this.literalMap.put(adapter.datatype(), adapter);
            return this;
        }

        public Builder with(String datatype, LinkedLiteralReader reader) {
            this.literalMap.put(datatype, reader);
            return this;
        }

        public JsonLdTreeReader build() {
            return new JsonLdTreeReader(fragmentMap.build(), literalMap);
        }
    }
}
