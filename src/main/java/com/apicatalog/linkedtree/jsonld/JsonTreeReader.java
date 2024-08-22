package com.apicatalog.linkedtree.jsonld;

import java.math.BigDecimal;
import java.text.DecimalFormat;
import java.text.DecimalFormatSymbols;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.Locale;
import java.util.Map;
import java.util.Map.Entry;

import com.apicatalog.linkedtree.LinkedContainer;
import com.apicatalog.linkedtree.LinkedFragment;
import com.apicatalog.linkedtree.LinkedLiteral;
import com.apicatalog.linkedtree.LinkedNode;
import com.apicatalog.linkedtree.LinkedTree;
import com.apicatalog.linkedtree.io.LinkedFragmentAdapter;
import com.apicatalog.linkedtree.io.LinkedLiteralAdapter;
import com.apicatalog.linkedtree.jsonld.primitive.JsonLdFragment;
import com.apicatalog.linkedtree.jsonld.primitive.JsonLdLiteral;
import com.apicatalog.linkedtree.primitive.GenericLink;
import com.apicatalog.linkedtree.primitive.GenericLinkedContainer;
import com.apicatalog.linkedtree.primitive.GenericLinkedTree;

import jakarta.json.JsonArray;
import jakarta.json.JsonNumber;
import jakarta.json.JsonObject;
import jakarta.json.JsonString;
import jakarta.json.JsonValue;
import jakarta.json.JsonValue.ValueType;

public class JsonTreeReader {

    protected static final DecimalFormat xsdNumberFormat =
            new DecimalFormat("0.0##############E0", new DecimalFormatSymbols(Locale.ENGLISH));

    static { xsdNumberFormat.setMinimumFractionDigits(1); }

    protected LinkedFragmentAdapter fragmentAdapter;
    protected Map<String, LinkedLiteralAdapter> literalAdapters;

    protected Map<String, GenericLink> links;

    public JsonTreeReader() {
        this.links = new HashMap<>();
        this.literalAdapters = new HashMap<>();
//        this.nodeReaders = new ArrayList<>();
//        this.valueReaders = new HashMap<>();
//        this.nodeWriters = new ArrayList<>();

//        nodeReaders.add(new JakartaNodeAdapter());
//        nodeWriters.add(new JakartaNodeWriter());
    }

    public LinkedTree readExpanded(JsonArray jsonNodes) {

        if (jsonNodes.isEmpty()) {
            return LinkedTree.EMPTY;
        }
        return GenericLinkedTree.of(readNodes(jsonNodes));
    }

    public Collection<LinkedNode> readNodes(final JsonArray jsonNodes) {

        if (jsonNodes.isEmpty()) {
            return Collections.emptyList();
        }

        final Collection<LinkedNode> nodes = new ArrayList<>(jsonNodes.size());

        for (final JsonValue jsonValue : jsonNodes) {

            if (jsonValue == null || !ValueType.OBJECT.equals(jsonValue.getValueType())) {
                throw new IllegalArgumentException();
            }
            nodes.add(readNode(jsonValue.asJsonObject()));
        }

        return nodes;
    }

    protected LinkedContainer readValueArray(JsonArray values) {

        final Collection<LinkedNode> data = new ArrayList<>(values.size());

        for (JsonValue item : values) {
            data.add(readValue(item));
        }

        return GenericLinkedContainer.of(LinkedContainer.Type.UnorderedSet, data);
    }

    protected LinkedNode readValue(JsonValue value) {

//      if (JsonUtils.isNotObject(value)) {
//      throw new DocumentError(ErrorType.Invalid, "Document");
//  }

        final JsonObject object = value.asJsonObject();

        return object.containsKey(Keywords.VALUE)
                ? readLiteral(object)
                : readNode(object);
    }
    
    protected LinkedNode readNode(JsonObject jsonObject) {

        if (isContainer(jsonObject, Keywords.LIST)) {
            return readList(jsonObject);
        }
        if (isContainer(jsonObject, Keywords.REVERSE)) {
            return readReverse(jsonObject);
        }        
        if (isContainer(jsonObject, Keywords.GRAPH)) {
            return readGraph(jsonObject);
        }
        
        return readFragment(jsonObject);
    }

    protected static boolean isContainer(JsonObject jsonObject, String name) {
        return jsonObject != null 
                && jsonObject.containsKey(name); 
    }

    // list is a collection attribute
    protected LinkedContainer readList(JsonObject jsonObject) {
        
        final JsonArray list = jsonObject.getJsonArray(Keywords.LIST);
        
        final Collection<LinkedNode> nodes = new ArrayList<>(list.size());
        
        for (JsonValue item : list) {
            nodes.add(readValue(item));
        }
        
        return GenericLinkedContainer.of(LinkedContainer.Type.OrderedList, nodes);
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
        
        final JsonArray graph = jsonObject.getJsonArray(Keywords.GRAPH);

        final Collection<LinkedNode> nodes = readNodes(graph);
        
        String id = null;
        Collection<String> types = Collections.emptySet();

        final Map<String, LinkedContainer> properties = new HashMap<>(jsonObject.size() - 1);

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
                throw new IllegalStateException("An unknown keyword " + entry.getKey());
                
            } else {
                properties.put(entry.getKey(), readValueArray(entry.getValue().asJsonArray()));
            }
        }

        if (id != null) {
            final GenericLink link = getOrCreate(id);
            final GenericLinkedTree node = GenericLinkedTree.of(
                    link,
                    types,
                    properties,
                    nodes
                    );
            link.add(node);
            return node;
        }

        //TODO read as a separate tree
        return GenericLinkedTree.of(null, types, properties, nodes);
    }

    protected LinkedFragment readFragment(JsonObject value) {

        String id = null;
        Collection<String> types = Collections.emptySet();
        String index = null;

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
                
            } else if ("@index".equals(entry.getKey())) {
                
                index = ((JsonString) entry.getValue()).getString();
                
            } else if (entry.getKey().startsWith("@")) {
                throw new IllegalStateException("An unknown keyword " + entry.getKey());
                
            } else {
                properties.put(entry.getKey(), readValueArray(entry.getValue().asJsonArray()));
            }
        }

        if (id != null) {
            final GenericLink link = getOrCreate(id);
            final JsonLdFragment node = JsonLdFragment.of(
                    link,
                    types,
                    properties,
                    index
                    );
            link.add(node);
            return node;
        }

        return JsonLdFragment.of(null, types, properties, index);
    }

    protected GenericLink getOrCreate(String uri) {

        GenericLink link = links.get(uri);
        if (link == null) {
            link = GenericLink.of(uri);
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

        // 6.
//        if (datatype != null && !"@json".equals(datatype) && !UriUtils.isAbsoluteUri(datatype, uriValidation)) {
//            LOGGER.log(Level.WARNING, "Datatype [{0}] is not an absolute IRI nor @json and value is skipped.", datatype);
//            return Optional.empty();
//        }

//        if (item.containsKey(Keywords.LANGUAGE)
//                && (JsonUtils.isNotString(item.get(Keywords.LANGUAGE))
//                        || !LanguageTag.isWellFormed(item.getString(Keywords.LANGUAGE)))) {
//            LOGGER.log(Level.WARNING, "Language tag [{0}] is not well formed string and value is skipped.", item.get(Keywords.LANGUAGE));
//            return Optional.empty();
//        }

        String valueString = null;

        if ("@json".equals(datatype)) {
//            valueString = JsonCanonicalizer.canonicalize(value);
//            datatype = RdfConstants.JSON;

        } else if (value != null && ValueType.TRUE.equals(value.getValueType())) {

            valueString = "true";

            if (datatype == null) {
                datatype = XsdConstants.BOOLEAN;
            }

        } else if (value != null && ValueType.FALSE.equals(value.getValueType())) {

            valueString = "false";

            if (datatype == null) {
                datatype = XsdConstants.BOOLEAN;
            }

        } else if (value != null && ValueType.NUMBER.equals(value.getValueType())) {

            JsonNumber number = ((JsonNumber) value);

            if ((!number.isIntegral() && number.doubleValue() % -1 != 0)
                    || XsdConstants.DOUBLE.equals(datatype)
                    || XsdConstants.FLOAT.equals(datatype)
                    || number.bigDecimalValue().compareTo(BigDecimal.ONE.movePointRight(21)) >= 0) {

                valueString = toXsdDouble(number.bigDecimalValue());

                if (datatype == null) {
                    datatype = XsdConstants.DOUBLE;
                }

                // 10.
            } else {

                valueString = number.bigIntegerValue().toString();

                if (datatype == null) {
                    datatype = XsdConstants.INTEGER;
                }

            }

            // 12.
        } else if (datatype == null) {

            datatype = XsdConstants.STRING;
        }

        if (valueString == null) {

            if (value == null || !ValueType.STRING.equals(value.getValueType())) {
                return null;
            }

            valueString = ((JsonString) value).getString();
        }

        String index = null;
        
        if (valueJsonObject.containsKey(Keywords.INDEX)) {
            index = valueJsonObject.getString(Keywords.INDEX);            
        }

        return JsonLdLiteral.of(valueString, datatype, getLiteralLanguage(valueJsonObject), null, index); // TODO

//        final JsonValue jsonValue = valueObject.get("@value");
//
//        if (jsonValue != null) {
//            switch (jsonValue.getValueType()) {
//            case FALSE:
//                return "false";
//
//            case TRUE:
//                return "true";
//
//            case STRING:
//                return ((JsonString) jsonValue).getString();
//
//            case NUMBER:
//                return ((JsonNumber) jsonValue).numberValue().toString();
//
//            case NULL:
//                return null;
//
//            default:
//                throw new IllegalArgumentException();
//            }
//        }
//        return null;
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

        final JsonValue jsonType = valueObject.get("@language");
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

    private static final String toXsdDouble(BigDecimal bigDecimal) {
        return xsdNumberFormat.format(bigDecimal);
    }

}
