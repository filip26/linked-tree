package com.apicatalog.linkedtree.orm.context;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.LinkedHashSet;
import java.util.LinkedList;
import java.util.Map;
import java.util.Objects;
import java.util.function.Predicate;

import com.apicatalog.jsonld.json.JsonUtils;
import com.apicatalog.linkedtree.jsonld.JsonLdContext;

import jakarta.json.Json;
import jakarta.json.JsonString;
import jakarta.json.JsonValue;

public class ContextReducer {

    Map<String, ContextDefinition> defs;

    public ContextReducer() {
        this.defs = new HashMap<>();
    }

    public JsonLdContext reduce(JsonLdContext... contexts) {
        if (contexts == null) {
            return null;
        }
        if (contexts.length < 2) {
            return contexts[0];
        }

        Collection<JsonValue> json = new LinkedHashSet<>();
        Collection<String> reduced = new LinkedHashSet<>();
        Collection<String> processed = new LinkedHashSet<>();

        Collection<ContextDefinition> prev = new LinkedList<>();

        for (final JsonLdContext jsonLdContext : contexts) {
            for (final JsonValue context : JsonUtils.toCollection(jsonLdContext.json())) {

                if (JsonUtils.isString(context)) {
                    final String contextUri = ((JsonString) context).getString();

                    if (processed.contains(contextUri)) {
                        continue;
                    }
                    
                    if (reduced.isEmpty() || !contains(prev, contextUri)) {
                        reduced.add(contextUri);
                        json.add(context);
                    }

                    final ContextDefinition def = defs.get(contextUri);
                    if (def != null) {
                        prev.add(def);
                    }

                    processed.add(contextUri);
                    
                } else if (!json.contains(context)){
                    json.add(context);
                }
            }
        }

        return new JsonLdContext(
                json.size() == 1
                        ? json.iterator().next()
                        : Json.createArrayBuilder(json).build(),
                reduced);
    }

    public Collection<String> reduce(Collection<String> context) {

        Objects.requireNonNull(context);

        if (context.size() < 2) {
            return context;
        }

        Collection<String> reduced = new ArrayList<>(context.size());
        ContextDefinition[] prev = new ContextDefinition[context.size()];
        int index = 0;

        for (String ctx : context) {
            if (index == 0 || !contains(prev, index, ctx)) {
                reduced.add(ctx);
            }
            prev[index++] = defs.get(ctx);
        }

        return reduced;
    }

    static boolean contains(ContextDefinition[] defs, int length, String context) {
        for (int i = 0; i < length; i++) {
            if (defs[i] != null && defs[i].includes().contains(context)) {
                return true;
            }
        }
        return false;
    }

    static boolean contains(Collection<ContextDefinition> defs, String context) {
        return defs.stream()
                .map(ContextDefinition::includes)
                .filter(Predicate.not(Collection::isEmpty))
                .anyMatch(d -> d.contains(context));
    }

    public ContextReducer define(
            String id,
            int position,
            Collection<String> includes) {

        this.defs.put(id, new ContextDefinition(id, position, includes));
        return this;
    }

    public ContextReducer define(
            String id,
            Collection<String> includes) {
        return define(id, -1, includes);
    }

}
