package com.apicatalog.linkedtree.json;

import java.net.URI;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;

import com.apicatalog.linkedtree.LinkedData;
import com.apicatalog.linkedtree.LinkedFragment;
import com.apicatalog.linkedtree.LinkedTree;
import com.apicatalog.linkedtree.LinkedValue;

import jakarta.json.JsonArray;
import jakarta.json.JsonObject;
import jakarta.json.JsonString;
import jakarta.json.JsonValue;
import jakarta.json.JsonValue.ValueType;

public class JsonTreeReader {

//    Collection<LinkedNodeAdapter<? extends LinkedFragment>> nodeReaders;
//    Map<String, LinkedValueAdapter<JsonValue, LinkedValue>> valueReaders;

//    Collection<LinkedDataWriter<JsonObject, JsonValue>> nodeWriters;

    protected Map<String, GenericLink> links;

    public JsonTreeReader() {
        this.links = new HashMap<>();
//        this.nodeReaders = new ArrayList<>();
//        this.valueReaders = new HashMap<>();
//        this.nodeWriters = new ArrayList<>();

//        nodeReaders.add(new JakartaNodeAdapter());
//        nodeWriters.add(new JakartaNodeWriter());
    }

    public LinkedTree read(JsonArray items) {

        if (items.isEmpty()) {
            return LinkedTree.EMPTY;
        }

        final Collection<LinkedFragment> fragments = new ArrayList<>(items.size());
        final JsonLinkedTree tree = new JsonLinkedTree(fragments);

        for (final JsonValue item : items) {

            if (item == null || !ValueType.OBJECT.equals(item.getValueType())) {
                throw new IllegalArgumentException();
            }
            fragments.add(readFragment(item.asJsonObject()));
        }

        return tree;
    }

    protected Collection<LinkedData> readValueArray(JsonArray values) {

        final Collection<LinkedData> data = new ArrayList<>(values.size());

        for (JsonValue item : values) {
            data.add(readValue(item));
        }

        return data;
    }

    protected LinkedData readValue(JsonValue value) {

//      if (JsonUtils.isNotObject(value)) {
//      throw new DocumentError(ErrorType.Invalid, "Document");
//  }

        final JsonObject object = value.asJsonObject();

        return object.containsKey("@value")
                ? readLiteral(object)
                : readFragment(object);

    }

    protected LinkedFragment readFragment(JsonObject value) {

        String id = null;

        final Map<String, Collection<LinkedData>> properties = new HashMap<>(value.size());

        for (final Entry<String, JsonValue> entry : value.entrySet()) {

            if ("@id".equals(entry.getKey())) {
                if (ValueType.STRING.equals(entry.getValue().getValueType())) {

                    final String idValue = ((JsonString) entry.getValue()).getString();

                    if (!idValue.startsWith("_:")) {
                        id = idValue;
                    }
                }

            } else if ("@type".equals(entry.getKey())) {
                // TODO

            } else if (entry.getKey().startsWith("@")) {

            } else {
                properties.put(entry.getKey(), readValueArray(entry.getValue().asJsonArray()));

            }
        }

        if (id != null) {
            final GenericLink link = getOrCreate(id);
            final GenericLinkedNode node = GenericLinkedNode.of(
                    link,
                    null,
                    properties);
            link.add(node);
            return node;
        }

        return GenericLinkedNode.of(null, null, properties);
    }

    protected GenericLink getOrCreate(String uri) {

        GenericLink link = links.get(uri);
        if (link == null) {
            link = new GenericLink(URI.create(uri));
            links.put(uri, link);
        }

        return link;
    }

    protected LinkedValue readLiteral(JsonObject value) {

        // for each adapter
        // adapter.read(value) == null contine;

//        return JsonLdLiteral.of();
        return null;
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

}
