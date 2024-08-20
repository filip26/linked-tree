package com.apicatalog.linkedtree.jakarta;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

import com.apicatalog.linkedtree.Link;
import com.apicatalog.linkedtree.LinkedData;
import com.apicatalog.linkedtree.LinkedFragment;
import com.apicatalog.linkedtree.LinkedValue;
import com.apicatalog.linkedtree.io.LinkedNodeAdapter;
import com.apicatalog.linkedtree.io.LinkedValueAdapter;

import jakarta.json.Json;
import jakarta.json.JsonArray;
import jakarta.json.JsonObject;
import jakarta.json.JsonValue;

public class JakartaLinkedTree {

    Collection<LinkedNodeAdapter<? extends LinkedFragment>> nodeReaders;
    Map<String, LinkedValueAdapter<JsonValue, LinkedValue>> valueReaders;

//    Collection<LinkedDataWriter<JsonObject, JsonValue>> nodeWriters;

    public JakartaLinkedTree() {
        this.nodeReaders = new ArrayList<>();
        this.valueReaders = new HashMap<>();
//        this.nodeWriters = new ArrayList<>();

//        nodeReaders.add(new JakartaNodeAdapter());
//        nodeWriters.add(new JakartaNodeWriter());
    }

    public Collection<LinkedFragment> read(JsonArray items) {

        if (items.isEmpty()) {
            return Collections.emptyList();
        }

        final Collection<LinkedFragment> results = new ArrayList<>(items.size());
//        final JakartaNodeContext ctx = new JakartaNodeContext();

        for (final JsonValue item : items) {

//          if (JsonUtils.isNotObject(value)) {
//          throw new DocumentError(ErrorType.Invalid, "Document");
//      }

//            results.add(genericObject(ctx, item.asJsonObject()));
        }

        return results;
    }

    protected LinkedData readObject(JsonValue value) {

//        if (JsonUtils.isNotObject(value)) {
//            throw new DocumentError(ErrorType.Invalid, "Document");
//        }

        final JsonObject object = value.asJsonObject();

//        return object.containsKey(Keywords.VALUE)
//                ? readLiteral(object)
//                : genericObject(object);
        return null;
    }

    protected LinkedValue readLiteral(JsonObject value) {

        // for each adapter
        // adapter.read(value) == null contine;

//        return JsonLdLiteral.of();
        return null;
    }

//    protected LinkedFragment genericObject(JakartaNodeContext context, JsonObject value) {
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

    public JsonArray write(Collection<LinkedFragment> tree) {

        var builder = Json.createArrayBuilder();

        for (var node : tree) {
            builder.add(write(node));
        }

        return builder.build();
    }

    private JsonValue write(LinkedFragment node) {

//        for (var writer : nodeWriters) {
//            if (writer.accepts(node)) {
//                return writer.write(null, node);
//            }
//        }

        throw new IllegalStateException();
    }

}
