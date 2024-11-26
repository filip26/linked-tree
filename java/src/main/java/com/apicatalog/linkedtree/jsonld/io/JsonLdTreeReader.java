package com.apicatalog.linkedtree.jsonld.io;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.stream.Stream;

import com.apicatalog.linkedtree.LinkedTree;
import com.apicatalog.linkedtree.adapter.NodeAdapterError;
import com.apicatalog.linkedtree.builder.TreeBuilderError;
import com.apicatalog.linkedtree.json.JsonDecimal;
import com.apicatalog.linkedtree.json.JsonInteger;
import com.apicatalog.linkedtree.json.JsonLiteral;
import com.apicatalog.linkedtree.json.JsonScalar;
import com.apicatalog.linkedtree.json.JsonTreeReader;
import com.apicatalog.linkedtree.json.pi.JsonObjectWrite;
import com.apicatalog.linkedtree.jsonld.JsonLdContext;
import com.apicatalog.linkedtree.jsonld.JsonLdId;
import com.apicatalog.linkedtree.jsonld.JsonLdKeyword;
import com.apicatalog.linkedtree.jsonld.JsonLdType;
import com.apicatalog.linkedtree.lang.LangStringLiteral.LanguageDirection;
import com.apicatalog.linkedtree.link.Link;
import com.apicatalog.linkedtree.link.MutableLink;
import com.apicatalog.linkedtree.literal.adapter.LiteralAdapter;
import com.apicatalog.linkedtree.orm.mapper.TreeReaderMapping;
import com.apicatalog.linkedtree.pi.ProcessingInstruction;
import com.apicatalog.linkedtree.traversal.NodeSelector;
import com.apicatalog.linkedtree.type.TypeAdapter;
import com.apicatalog.linkedtree.xsd.XsdVocab;

import jakarta.json.JsonArray;
import jakarta.json.JsonNumber;
import jakarta.json.JsonObject;
import jakarta.json.JsonString;
import jakarta.json.JsonStructure;
import jakarta.json.JsonValue;
import jakarta.json.JsonValue.ValueType;

public class JsonLdTreeReader extends JsonTreeReader {

    static final JsonLdTreeReader GENERIC = new JsonLdTreeReader(Collections.emptyMap(), Collections.emptyMap(), Collections.emptyMap());

    Collection<String> context;
    Map<Class<?>, TypeAdapter> typeAdapters;

    JsonLdTreeReader(
            Map<Class<?>, TypeAdapter> typeAdapters,
            Map<String, TypeAdapter> typeMap,
            Map<String, LiteralAdapter> literalAdapters) {
        super(typeMap, literalAdapters);
        this.typeAdapters = typeAdapters;
    }

    /**
     * A generic instance with no adapters attached.
     * 
     * @return a generic instance
     */
    public static final JsonLdTreeReader generic() {
        return GENERIC;
    }

    public static final JsonLdTreeReader of(TreeReaderMapping mapping) {
        return new JsonLdTreeReader(mapping.typeAdapters(), mapping.fragmentAdapters(), mapping.literalAdapters());
    }

    public <T> T read(Class<T> clazz, JsonArray expanded) throws TreeBuilderError, NodeAdapterError {
        Objects.requireNonNull(clazz);
        Objects.requireNonNull(expanded);

        return read(clazz, Collections.emptyList(), expanded);
    }

    @SuppressWarnings("unchecked")
    public <T> T read(Class<T> clazz, List<String> context, JsonArray expanded) throws TreeBuilderError, NodeAdapterError {

        LinkedTree tree = read(context, expanded);
        if (tree == null || tree.size() == 0) {
            return null;
        }

        if (tree.type().isAdaptableTo(clazz)
                || (tree.size() == 1 && tree.node().isFragment()
                        && tree.node().asFragment().type().isAdaptableTo(clazz))) {
            return tree.materialize(clazz);
        }

        if (typeAdapters.containsKey(clazz) && (tree.size() == 1 && tree.node().isFragment())) {
            return (T) typeAdapters.get(clazz).materialize(tree.fragment());
        }

        throw new ClassCastException("Cannot cast " + tree.type().stream().toList() + " to " + clazz);
    }

    public LinkedTree read(JsonStructure source) throws TreeBuilderError {
        return read(Collections.emptyList(), source);
    }

    public LinkedTree read(Collection<String> context, JsonStructure source) throws TreeBuilderError {
        this.context = context != null ? context : Collections.emptyList();
        return read(source, ((node, indexOrder, indexTerm, depth) -> TraversalPolicy.Accept));
    }

    public LinkedTree read(Collection<String> context, JsonStructure source, NodeSelector<JsonValue> selector) throws TreeBuilderError {
        this.context = context != null ? context : Collections.emptyList();
        return read(source, selector);
    }

    @Override
    public LinkedTree read(JsonStructure source, NodeSelector<JsonValue> selector) throws TreeBuilderError {
        return super.read(source, (node, indexOrder, indexTerm, depth) -> {

            if (ValueType.OBJECT == node.getValueType()) {

                // do not follow @value objects
                if (node.asJsonObject().containsKey(JsonLdKeyword.VALUE)) {
                    return TraversalPolicy.Stop;
                }
            }

            if (JsonLdKeyword.GRAPH.equals(indexTerm)) {
                return TraversalPolicy.Ignore;
            }

            if (JsonLdKeyword.LIST.equals(indexTerm)) {
                return TraversalPolicy.Ignore;
            }

            if (indexTerm != null
                    && indexTerm.startsWith("@")
                    && indexTerm.length() > 1) {
                return TraversalPolicy.Drop;
            }

            return selector.test(node, indexOrder, indexTerm, depth);
        });

    }

    @Override
    protected void process(final JsonValue source) throws TreeBuilderError {
        if (ValueType.OBJECT == source.getValueType()) {
            // tree
            if (source.asJsonObject().containsKey(JsonLdKeyword.GRAPH)) {
                jsonLdTree(source.asJsonObject(), Collections.emptyList());
                return;

                // literal
            } else if (source.asJsonObject().containsKey(JsonLdKeyword.VALUE)) {
                jsonLdLiteral(source.asJsonObject(), new ArrayList<>());
                return;

                // list
            } else if (source.asJsonObject().containsKey(JsonLdKeyword.LIST)) {
                list(source.asJsonObject().getJsonArray(JsonLdKeyword.LIST).size());
                return;
            }
            // fragment
            jsonLdFragment(source.asJsonObject());
            return;
        }

        // container or root tree
        if (ValueType.ARRAY == source.getValueType()) {
            if (trees.isEmpty()) {
                cloneRoot(source.asJsonArray());
                return;
            }

            container(source.asJsonArray().size());
            return;
        }

        throw new IllegalArgumentException("The input is not an expanded JSON-LD, expected JSON object or an array but got " + source);
    }

    protected void jsonLdTree(final JsonObject source, Collection<ProcessingInstruction> ops) {
        tree(
                JsonLdId.string(source),
                JsonLdType.strings(source),
                source.size(),
                source.get(JsonLdKeyword.GRAPH) != null
                        ? source.getJsonArray(JsonLdKeyword.GRAPH).size()
                        : 0,
                Stream.concat(
                        ops.stream(),
                        ops(source, List.of(JsonLdKeyword.ID, JsonLdKeyword.TYPE, JsonLdKeyword.GRAPH)).stream()).toList());
    }

    protected void cloneRoot(final JsonArray source) {

        final Collection<ProcessingInstruction> ops = context == null || context.isEmpty()
                ? Collections.emptyList()
                : List.of(JsonLdContext.of(context));

        if (source.size() == 1
                && ValueType.OBJECT == source.iterator().next().getValueType()
                && source.iterator().next().asJsonObject().containsKey(JsonLdKeyword.GRAPH)) {
            jsonLdTree(source.iterator().next().asJsonObject(), ops);
            return;
        }

        tree(
                null,
                Collections.emptySet(),
                0,
                source.size(),
                ops);
    }

    protected void jsonLdFragment(final JsonObject source) {
        fragment(
                JsonLdId.string(source),
                JsonLdType.strings(source),
                source.size(),
                ops(source, List.of(JsonLdKeyword.ID, JsonLdKeyword.TYPE)));
    }

    protected Collection<ProcessingInstruction> ops(JsonObject source, Collection<String> filter) {
        if (source.isEmpty()) {
            return Collections.emptyList();
        }
        // identify unknown keywords
        Map<String, JsonValue> unknown = new LinkedHashMap<>();

        source.entrySet()
                .stream()
                .filter(e -> e.getKey().startsWith("@")
                        && e.getKey().length() > 1
                        && !filter.contains(e.getKey()))
                .forEach(e -> unknown.put(e.getKey(), e.getValue()));

        if (!unknown.isEmpty()) {
            List<ProcessingInstruction> ops = new ArrayList<>();
            ops.add(new JsonObjectWrite(unknown));
            return ops;
        }
        return Collections.emptyList();
    }

    protected static MutableLink getOrCreate(String uri, Map<String, Link> links) {

        MutableLink link = (MutableLink) links.get(uri);
        if (link == null) {
            link = MutableLink.of(uri);
            links.put(uri, link);
        }

        return link;
    }

    protected void jsonLdLiteral(final JsonObject valueJsonObject, final Collection<ProcessingInstruction> ops) throws TreeBuilderError {

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
            pi(ops);
            literal(JsonLiteral.of(value));
            return;

        } else if (value != null &&
                (ValueType.TRUE.equals(value.getValueType())
                        || ValueType.FALSE.equals(value.getValueType()))) {

            var pi = getPi(valueJsonObject, JsonLdKeyword.TYPE, JsonLdKeyword.VALUE);
            if (pi != null) {
                ops.add(pi);
            }
            pi(ops);
            literal(new JsonScalar(value,
                    datatype != null
                            ? datatype
                            : XsdVocab.BOOLEAN,
                    root()));
            return;

        } else if (value != null && ValueType.NUMBER.equals(value.getValueType())) {

            JsonNumber number = ((JsonNumber) value);

            if ((!number.isIntegral() && number.doubleValue() % -1 != 0)
                    || XsdVocab.DOUBLE.equals(datatype)
                    || XsdVocab.FLOAT.equals(datatype)
                    || number.bigDecimalValue().compareTo(BigDecimal.ONE.movePointRight(21)) >= 0) {

                var pi = getPi(valueJsonObject, JsonLdKeyword.TYPE, JsonLdKeyword.VALUE);
                if (pi != null) {
                    ops.add(pi);
                }
                pi(ops);
                literal(JsonDecimal.of(number,
                        datatype != null
                                ? datatype
                                : XsdVocab.DOUBLE));
                return;

            } else {
                var pi = getPi(valueJsonObject, JsonLdKeyword.TYPE, JsonLdKeyword.VALUE);
                if (pi != null) {
                    ops.add(pi);
                }
                pi(ops);
                literal(JsonInteger.of(
                        number,
                        datatype != null
                                ? datatype
                                : XsdVocab.INTEGER));
                return;
            }

        }

        if (value == null || !ValueType.STRING.equals(value.getValueType())) {
            return;
        }

        final String valueString = ((JsonString) value).getString();

        if (datatype != null
                && literalAdapters != null
                && !literalAdapters.isEmpty()) {

            final LiteralAdapter adapter = literalAdapters.get(datatype);

            if (adapter != null) {
                try {
                    var pi = getPi(valueJsonObject, JsonLdKeyword.VALUE, JsonLdKeyword.TYPE);
                    if (pi != null) {
                        ops.add(pi);
                    }
                    pi(ops);
                    literal(adapter.materialize(valueString));
                    return;
                } catch (NodeAdapterError e) {
                    throw new TreeBuilderError(e);
                }
            }
        }

        if (datatype == null) {
            datatype = XsdVocab.STRING;
        }

        if (XsdVocab.STRING.equals(datatype)) {
            var pi = getPi(valueJsonObject, JsonLdKeyword.VALUE, JsonLdKeyword.TYPE, JsonLdKeyword.LANGUAGE, JsonLdKeyword.DIRECTION);
            if (pi != null) {
                ops.add(pi);
            }

            immutableLangString(
                    valueString,
                    getLiteralLanguage(valueJsonObject),
                    getLiteralDirection(valueJsonObject),
                    ops);
            return;
        }

        var pi = getPi(valueJsonObject, JsonLdKeyword.VALUE, JsonLdKeyword.TYPE);
        if (pi != null) {
            ops.add(pi);
        }

        immutableLiteral(
                valueString,
                datatype,
                ops);
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
        final JsonValue jsonType = valueObject.get(JsonLdKeyword.TYPE);
        if (jsonType == null || ValueType.NULL.equals(jsonType.getValueType())) {
            return null;
        }
        if (jsonType instanceof JsonString string) {
            return string.getString();
        }
        throw new IllegalArgumentException();
    }

    protected static String getLiteralLanguage(JsonObject valueObject) {
        final JsonValue jsonType = valueObject.get(JsonLdKeyword.LANGUAGE);
        if (jsonType == null || ValueType.NULL.equals(jsonType.getValueType())) {
            return null;
        }
        if (jsonType instanceof JsonString string) {
            return string.getString();
        }
        throw new IllegalArgumentException();
    }

    protected static LanguageDirection getLiteralDirection(JsonObject valueObject) {
        final JsonValue jsonType = valueObject.get(JsonLdKeyword.DIRECTION);
        if (jsonType == null || ValueType.NULL.equals(jsonType.getValueType())) {
            return null;
        }
        if (jsonType instanceof JsonString string) {
            return switch (string.getString().toLowerCase()) {
            case "ltr" -> LanguageDirection.LTR;
            case "rtl" -> LanguageDirection.RTL;
            default -> null;
            };
        }
        throw new IllegalArgumentException();
    }
}
