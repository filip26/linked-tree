package com.apicatalog.linkedtree.jsonld.io;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import com.apicatalog.linkedtree.LinkedTree;
import com.apicatalog.linkedtree.adapter.AdapterError;
import com.apicatalog.linkedtree.builder.TreeBuilderError;
import com.apicatalog.linkedtree.json.JsonDecimal;
import com.apicatalog.linkedtree.json.JsonInteger;
import com.apicatalog.linkedtree.json.JsonLiteral;
import com.apicatalog.linkedtree.json.JsonScalar;
import com.apicatalog.linkedtree.json.JsonTreeReader;
import com.apicatalog.linkedtree.json.pi.JsonObjectWrite;
import com.apicatalog.linkedtree.jsonld.JsonLdId;
import com.apicatalog.linkedtree.jsonld.JsonLdKeyword;
import com.apicatalog.linkedtree.jsonld.JsonLdType;
import com.apicatalog.linkedtree.link.Link;
import com.apicatalog.linkedtree.link.MutableLink;
import com.apicatalog.linkedtree.literal.adapter.LiteralAdapter;
import com.apicatalog.linkedtree.pi.ProcessingInstruction;
import com.apicatalog.linkedtree.traversal.NodeSelector;
import com.apicatalog.linkedtree.type.TypeAdapter;
import com.apicatalog.linkedtree.xsd.XsdConstants;

import jakarta.json.JsonArray;
import jakarta.json.JsonNumber;
import jakarta.json.JsonObject;
import jakarta.json.JsonString;
import jakarta.json.JsonStructure;
import jakarta.json.JsonValue;
import jakarta.json.JsonValue.ValueType;

public class JsonLdTreeReader extends JsonTreeReader {

    protected JsonLdTreeReader(
            Map<String, TypeAdapter> typeAdapters,
            Map<String, LiteralAdapter> literalAdapters) {
        super(typeAdapters, literalAdapters);
    }

    public LinkedTree read(JsonStructure source) throws TreeBuilderError {
        return read(Collections.emptyList(), source);
    }

    public LinkedTree read(List<String> context, JsonStructure source) throws TreeBuilderError {
        // TODO context
        return read(source, ((node, indexOrder, indexTerm, depth) -> ProcessingPolicy.Accept));
    }

    @Override
    public LinkedTree read(JsonStructure source, NodeSelector<JsonValue> selector) throws TreeBuilderError {
        return super.read(source, (node, indexOrder, indexTerm, depth) -> {

            if (ValueType.OBJECT == node.getValueType()) {

                // do not follow @value objects
                if (node.asJsonObject().containsKey(JsonLdKeyword.VALUE)) {
                    return ProcessingPolicy.Stop;
                }
            }

            if (JsonLdKeyword.GRAPH.equals(indexTerm)) {
                return ProcessingPolicy.Ignore;
            }

            if (JsonLdKeyword.LIST.equals(indexTerm)) {
                return ProcessingPolicy.Ignore;
            }

            if (indexTerm != null
                    && indexTerm.startsWith("@")
                    && indexTerm.length() > 1) {
                return ProcessingPolicy.Drop;
            }

            return selector.test(node, indexOrder, indexTerm, depth);
        });

    }

    @Override
    protected void process(final JsonValue source) throws TreeBuilderError {
        if (ValueType.OBJECT == source.getValueType()) {
            // tree
            if (source.asJsonObject().containsKey(JsonLdKeyword.GRAPH)) {
                jsonLdTree(source.asJsonObject());
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

    protected void jsonLdTree(final JsonObject source) {
        tree(
                JsonLdId.string(source),
                JsonLdType.strings(source),
                source.size(),
                source.get(JsonLdKeyword.GRAPH) != null
                        ? source.getJsonArray(JsonLdKeyword.GRAPH).size()
                        : 0,
                ops(source, List.of(JsonLdKeyword.ID, JsonLdKeyword.TYPE, JsonLdKeyword.GRAPH)));
    }

    protected void cloneRoot(final JsonArray source) {

        if (source.size() == 1
                && ValueType.OBJECT == source.iterator().next().getValueType()
                && source.iterator().next().asJsonObject().containsKey(JsonLdKeyword.GRAPH)) {
            jsonLdTree(source.iterator().next().asJsonObject());
            return;
        }

        tree(
                null,
                Collections.emptySet(),
                0,
                source.size(),
                Collections.emptyList());
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
                            : XsdConstants.BOOLEAN));
            return;

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
                pi(ops);
                literal(JsonDecimal.of(number,
                        datatype != null
                                ? datatype
                                : XsdConstants.DOUBLE));
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
                                : XsdConstants.INTEGER));
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
                    literal(adapter.materialize(valueString, root()));
                    return;
                } catch (AdapterError e) {
                    throw new TreeBuilderError(e);
                }
            }
        }

        if (datatype == null) {
            datatype = XsdConstants.STRING;
        }

        if (XsdConstants.STRING.equals(datatype)) {
            // TODO direction
            var pi = getPi(valueJsonObject, JsonLdKeyword.VALUE, JsonLdKeyword.TYPE, JsonLdKeyword.LANGUAGE);
            if (pi != null) {
                ops.add(pi);
            }

            immutableLangString(
                    valueString,
                    getLiteralLanguage(valueJsonObject),
                    null, // FIXME direction
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

    public static final Builder create() {
        return new Builder();
    }

    public static class Builder {

        protected Map<String, TypeAdapter> typeMap;
        protected Map<String, LiteralAdapter> literalMap;

        public Builder() {
            this.typeMap = new LinkedHashMap<>();
            this.literalMap = new LinkedHashMap<>();
        }

        public Builder with(String type, TypeAdapter adapter) {
            this.typeMap.put(type, adapter);
            return this;
        }

        public Builder with(LiteralAdapter adapter) {
            this.literalMap.put(adapter.datatype(), adapter);
            return this;
        }

        public Builder with(String datatype, LiteralAdapter adapter) {
            this.literalMap.put(datatype, adapter);
            return this;
        }

        public JsonLdTreeReader build() {
            return new JsonLdTreeReader(typeMap, literalMap);
        }
    }
}
