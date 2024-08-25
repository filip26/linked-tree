package com.apicatalog.linkedtree.jsonld.io;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Map.Entry;
import java.util.function.Function;
import java.util.stream.Collectors;

import com.apicatalog.linkedtree.LinkedContainer;
import com.apicatalog.linkedtree.LinkedFragment;
import com.apicatalog.linkedtree.LinkedLiteral;
import com.apicatalog.linkedtree.LinkedNode;
import com.apicatalog.linkedtree.LinkedTree;
import com.apicatalog.linkedtree.adapter.FragmentAdapterTypeMap;
import com.apicatalog.linkedtree.adapter.LinkedFragmentAdapter;
import com.apicatalog.linkedtree.adapter.LinkedFragmentReader;
import com.apicatalog.linkedtree.adapter.LinkedLiteralAdapter;
import com.apicatalog.linkedtree.adapter.LinkedLiteralReader;
import com.apicatalog.linkedtree.json.JsonDecimal;
import com.apicatalog.linkedtree.json.JsonInteger;
import com.apicatalog.linkedtree.json.JsonLiteral;
import com.apicatalog.linkedtree.json.JsonScalar;
import com.apicatalog.linkedtree.jsonld.JsonLdKeyword;
import com.apicatalog.linkedtree.lang.ImmutableLangString;
import com.apicatalog.linkedtree.link.Link;
import com.apicatalog.linkedtree.link.MutableLink;
import com.apicatalog.linkedtree.pi.ProcessingInstruction;
import com.apicatalog.linkedtree.primitive.GenericLinkedContainer;
import com.apicatalog.linkedtree.primitive.GenericLinkedFragment;
import com.apicatalog.linkedtree.primitive.GenericLinkedLiteral;
import com.apicatalog.linkedtree.primitive.GenericLinkedTree;
import com.apicatalog.linkedtree.xsd.XsdConstants;

import jakarta.json.JsonArray;
import jakarta.json.JsonNumber;
import jakarta.json.JsonObject;
import jakarta.json.JsonString;
import jakarta.json.JsonValue;
import jakarta.json.JsonValue.ValueType;

public class JsonLdTreeReader {

    protected LinkedFragmentAdapter fragmentAdapter;
    protected Map<String, LinkedLiteralReader> literalAdapters;

    protected JsonLdTreeReader(LinkedFragmentAdapter fragmentAdapter, Map<String, LinkedLiteralReader> literalAdapters) {
        this.fragmentAdapter = fragmentAdapter;
        this.literalAdapters = literalAdapters;
    }

    public static JsonLdTreeReader with(LinkedLiteralAdapter... literalAdapters) {
        return with(null, literalAdapters);
    }

    public static JsonLdTreeReader with(LinkedFragmentAdapter fragmentAdapter, LinkedLiteralAdapter... literalAdapters) {
        return new JsonLdTreeReader(fragmentAdapter, literalAdapters != null
                ? Arrays.stream(literalAdapters)
                        .collect(
                                Collectors.toMap(
                                        LinkedLiteralAdapter::datatype,
                                        Function.identity()))
                : Collections.emptyMap());
    }

    public LinkedTree readExpanded(JsonArray jsonNodes) {

        if (jsonNodes.isEmpty()) {
            return LinkedTree.EMPTY;
        }

        final Map<String, Link> links = new HashMap<>();

        final LinkedTree tree = GenericLinkedTree.of(readNodes(jsonNodes, links), links);

        for (final Link link : links.values()) {
            ((MutableLink) link).target(adapt(((MutableLink) link), mergeTypes(link.fragments()), merge(link.fragments()), null));
        }

        return tree;
    }

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
        if (fragment instanceof GenericLinkedFragment generic) {
            map.putAll(generic.entries());
        }
        for (final String term : fragment.terms()) {
            map.put(term, fragment.property(term));
        }
        return map;
    }

    public Collection<LinkedNode> readNodes(final JsonArray jsonNodes, Map<String, Link> links) {

        if (jsonNodes.isEmpty()) {
            return Collections.emptyList();
        }

        final Collection<LinkedNode> nodes = new ArrayList<>(jsonNodes.size());

        for (final JsonValue jsonValue : jsonNodes) {

            if (jsonValue == null || !ValueType.OBJECT.equals(jsonValue.getValueType())) {
                throw new IllegalArgumentException();
            }
            nodes.add(readNode(jsonValue.asJsonObject(), links));
        }

        return nodes;
    }

    protected LinkedContainer readValueArray(JsonArray values, Map<String, Link> links) {

        final Collection<LinkedNode> data = new ArrayList<>(values.size());

        for (final JsonValue item : values) {
            data.add(readValue(item, links));
        }

        return new GenericLinkedContainer(LinkedContainer.Type.UnorderedSet, data, null);
    }

    protected LinkedNode readValue(JsonValue value, Map<String, Link> links) {

//      if (JsonUtils.isNotObject(value)) {
//      throw new DocumentError(ErrorType.Invalid, "Document");
//  }

        final JsonObject object = value.asJsonObject();

        return object.containsKey(JsonLdKeyword.VALUE)
                ? readLiteral(object)
                : readNode(object, links);
    }

    protected LinkedNode readNode(JsonObject jsonObject, Map<String, Link> links) {

        if (isContainer(jsonObject, JsonLdKeyword.LIST)) {
            return readList(jsonObject, links);
        }
//        if (isContainer(jsonObject, Keywords.REVERSE)) {
//            return readReverse(jsonObject);
//        }        
        if (isContainer(jsonObject, JsonLdKeyword.GRAPH)) {
            return readGraph(jsonObject);
        }

        return readFragment(jsonObject, links);
    }

    protected static boolean isContainer(JsonObject jsonObject, String name) {
        return jsonObject != null
                && jsonObject.containsKey(name);
    }

    // list is a collection attribute
    protected LinkedContainer readList(JsonObject jsonObject, Map<String, Link> links) {

        final JsonArray list = jsonObject.getJsonArray(JsonLdKeyword.LIST);

        final Collection<LinkedNode> nodes = new ArrayList<>(list.size());

        for (final JsonValue item : list) {
            nodes.add(readValue(item, links));
        }

        return new GenericLinkedContainer(LinkedContainer.Type.OrderedList, nodes, null);
    }

    protected LinkedContainer readReverse(JsonObject jsonObject) {

//        final JsonArray list = jsonObject.getJsonObject(Keywords.REVERSE);

//        final Collection<LinkedNode> nodes = new ArrayList<>(list.size());

//        for (JsonValue item : list) {
//            nodes.add(readValue(item));
//        }

//        return GenericLinkedContainer.of(Keywords.LIST, nodes);
        return null;
    }

    protected LinkedTree readGraph(JsonObject jsonObject) {

        final JsonArray graph = jsonObject.getJsonArray(JsonLdKeyword.GRAPH);

        final Map<String, Link> links = new HashMap<>();

        final Collection<LinkedNode> nodes = readNodes(graph, links);

        String id = null;
        Collection<String> types = Collections.emptySet();

        final Map<String, LinkedContainer> properties = new HashMap<>(jsonObject.size() - 1);
        Map<String, JsonValue> meta = new HashMap<>();

        for (final Entry<String, JsonValue> entry : jsonObject.entrySet()) {

            if ("@graph".equals(entry.getKey())) {
                continue;
            }

            if ("@id".equals(entry.getKey())) {
                if (ValueType.STRING.equals(entry.getValue().getValueType())) {

                    id = ((JsonString) entry.getValue()).getString();

//                    if (!idValue.startsWith("_:")) {
//                        id = idValue;
//                    }
                }

            } else if ("@type".equals(entry.getKey())) {

                types = entry.getValue().asJsonArray().stream().map(JsonString.class::cast)
                        .map(JsonString::getString)
                        .toList();

            } else if (entry.getKey().startsWith("@")) {
                meta.put(entry.getKey(), entry.getValue());

            } else {
                properties.put(entry.getKey(), readValueArray(entry.getValue().asJsonArray(), links));
            }
        }

        for (final Link link : links.values()) {
            ((MutableLink) link).target(adapt(((MutableLink) link), mergeTypes(link.fragments()), merge(link.fragments()), null));
        }

        if (id != null) {
            final MutableLink link = getOrCreate(id, links);
            final GenericLinkedTree node = new GenericLinkedTree(
                    link,
                    types,
                    properties,
                    nodes,
                    links,
                    new JsonLdPi(meta));
            link.addFragment(node);
            return node;
        }

        return new GenericLinkedTree(null, types, properties, nodes, links, new JsonLdPi(meta));
    }

    protected LinkedFragment readFragment(JsonObject value, Map<String, Link> links) {

        String id = null;
        Collection<String> types = Collections.emptySet();
        Map<String, JsonValue> meta = new HashMap<>();

        final Map<String, LinkedContainer> properties = new HashMap<>(value.size());

        for (final Entry<String, JsonValue> entry : value.entrySet()) {

            if ("@id".equals(entry.getKey())) {
                if (ValueType.STRING.equals(entry.getValue().getValueType())) {

                    id = ((JsonString) entry.getValue()).getString();

//                    if (!idValue.startsWith("_:")) {
//                        id = idValue;
//                    }
                }

            } else if ("@type".equals(entry.getKey())) {

                types = entry.getValue().asJsonArray().stream().map(JsonString.class::cast)
                        .map(JsonString::getString)
                        .toList();

            } else if (entry.getKey().startsWith("@")) {
                meta.put(entry.getKey(), entry.getValue());

            } else {
                properties.put(entry.getKey(), readValueArray(entry.getValue().asJsonArray(), links));
            }
        }

        if (id != null) {
            final MutableLink link = getOrCreate(id, links);
            final GenericLinkedFragment node = new GenericLinkedFragment(
                    link,
                    types,
                    properties,
                    new JsonLdPi(meta));
            link.addFragment(node);
            return node;
        }

        return adapt(null, types, properties, new JsonLdPi(meta));
    }

    protected LinkedFragment adapt(MutableLink id, Collection<String> type, Map<String, LinkedContainer> data, ProcessingInstruction pi) {

        if (fragmentAdapter != null && fragmentAdapter.accepts(id != null ? id.uri() : null, type)) {
            final LinkedFragment fragment = fragmentAdapter.read(id, type, data, pi);
            if (fragment != null) {
                return fragment;
            }
        }

        return new GenericLinkedFragment(
                id,
                type,
                data,
                pi);
    }

    protected static MutableLink getOrCreate(String uri, Map<String, Link> links) {

        MutableLink link = (MutableLink) links.get(uri);
        if (link == null) {
            link = MutableLink.of(uri);
            links.put(uri, link);
        }

        return link;
    }

    protected LinkedLiteral readLiteral(final JsonObject valueJsonObject) {

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

        final JsonValue value = valueJsonObject.get("@value");

        String datatype = valueJsonObject.containsKey("@type")
                && ValueType.STRING.equals(valueJsonObject.get("@type").getValueType())
                        ? valueJsonObject.getString("@type")
                        : null;

        if (JsonLdKeyword.JSON.equals(datatype)) {
            return JsonLiteral.of(value, getPi(valueJsonObject, JsonLdKeyword.VALUE, JsonLdKeyword.TYPE));

        } else if (value != null &&
                (ValueType.TRUE.equals(value.getValueType())
                        || ValueType.FALSE.equals(value.getValueType()))) {

            return new JsonScalar(value,
                    datatype != null
                            ? datatype
                            : XsdConstants.BOOLEAN,
                    getPi(valueJsonObject, JsonLdKeyword.TYPE, JsonLdKeyword.VALUE));

        } else if (value != null && ValueType.NUMBER.equals(value.getValueType())) {

            JsonNumber number = ((JsonNumber) value);

            if ((!number.isIntegral() && number.doubleValue() % -1 != 0)
                    || XsdConstants.DOUBLE.equals(datatype)
                    || XsdConstants.FLOAT.equals(datatype)
                    || number.bigDecimalValue().compareTo(BigDecimal.ONE.movePointRight(21)) >= 0) {

                return JsonDecimal.of(number,
                        datatype != null
                                ? datatype
                                : XsdConstants.DOUBLE,
                        getPi(valueJsonObject, JsonLdKeyword.TYPE, JsonLdKeyword.VALUE));

            } else {
                return JsonInteger.of(
                        number,
                        datatype != null
                                ? datatype
                                : XsdConstants.INTEGER,
                        getPi(valueJsonObject, JsonLdKeyword.TYPE, JsonLdKeyword.VALUE));
            }

        }

        if (value == null || !ValueType.STRING.equals(value.getValueType())) {
            return null;
        }

        if (datatype == null) {
            datatype = XsdConstants.STRING;
        }

        String valueString = ((JsonString) value).getString();

        final LinkedLiteralReader adapter = literalAdapters.get(datatype);
        if (adapter != null) {
            return adapter.read(valueString, getPi(valueJsonObject, JsonLdKeyword.VALUE, JsonLdKeyword.TYPE));
        }

        if (XsdConstants.STRING.equals(datatype)) {
            // TODO direction
            return new ImmutableLangString(
                    valueString,
                    getLiteralLanguage(valueJsonObject),
                    null,
                    getPi(valueJsonObject, JsonLdKeyword.VALUE, JsonLdKeyword.TYPE, JsonLdKeyword.LANGUAGE));
        }

        return new GenericLinkedLiteral(
                valueString,
                datatype,
                getPi(valueJsonObject, JsonLdKeyword.VALUE, JsonLdKeyword.TYPE));
    }

    protected static JsonLdPi getPi(JsonObject valueJsonObject, String... filter) {
        final Map<String, JsonValue> pi = new HashMap<>();

        for (final Map.Entry<String, JsonValue> jsonEntry : valueJsonObject.entrySet()) {
            if (JsonLdKeyword.anyMatch(jsonEntry.getKey(), filter)) {
                continue;
            }
            pi.put(jsonEntry.getKey(), jsonEntry.getValue());
        }
        return new JsonLdPi(pi);
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

        protected Map<String, LinkedFragmentReader> fragmentMap;
        protected Map<String, LinkedLiteralReader> literalMap;

        public Builder() {
            this.fragmentMap = new LinkedHashMap<>();
            this.literalMap = new LinkedHashMap<>();
        }

        public Builder with(String type, LinkedFragmentReader reader) {
            this.fragmentMap.put(type, reader);
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
            return new JsonLdTreeReader(new FragmentAdapterTypeMap(fragmentMap), literalMap);
        }

    }

}
