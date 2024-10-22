package com.apicatalog.linkedtree.jsonld.io;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.Map;
import java.util.Objects;
import java.util.Set;

import com.apicatalog.linkedtree.def.PropertyDefinition;
import com.apicatalog.linkedtree.def.TypeDefinition;
import com.apicatalog.linkedtree.jsonld.JsonLdKeyword;
import com.apicatalog.linkedtree.literal.adapter.DataTypeNormalizer;
import com.apicatalog.linkedtree.orm.Context;
import com.apicatalog.linkedtree.orm.Fragment;
import com.apicatalog.linkedtree.orm.Id;
import com.apicatalog.linkedtree.orm.Literal;
import com.apicatalog.linkedtree.orm.Term;
import com.apicatalog.linkedtree.orm.Vocab;
import com.apicatalog.linkedtree.type.Type;

import jakarta.json.Json;
import jakarta.json.JsonObject;
import jakarta.json.JsonObjectBuilder;
import jakarta.json.JsonValue;

public class ObjectJsonLdWriter {

    Map<Class<?>, TypeDefinition> fragments;
    Map<Class<?>, DataTypeNormalizer<?>> datatypes;

    public ObjectJsonLdWriter() {
        this.fragments = new HashMap<>();
        this.datatypes = new HashMap<>();
    }

    public ObjectJsonLdWriter scan(Class<?> type) {

        Objects.requireNonNull(type);

        if (!type.isInterface()) {
            throw new IllegalArgumentException();
        }

        scanInterface(type);

        return this;
    }

    void scanInterface(Class<?> typeInterface) {
        Fragment fragment = typeInterface.getDeclaredAnnotation(Fragment.class);
        if (fragment == null) {
            // TODO log
            return;
        }

        Context fragmentContext = typeInterface.getAnnotation(Context.class);

        Collection<String> context = Collections.emptySet();

        if (fragmentContext != null) {
            context = Set.of(fragmentContext.value());
        }

        String name = null;

        PropertyDefinition type = null;

        // process type
        if (!fragment.generic()) {
            name = typeInterface.getSimpleName();

            Term fragmentTerm = typeInterface.getAnnotation(Term.class);

            if (fragmentTerm != null) {
                name = fragmentTerm.value();
            }
        }

        String vocab = null;
        Vocab fragmentVocab = typeInterface.getAnnotation(Vocab.class);
        if (fragmentVocab != null) {
            vocab = fragmentVocab.value();
        }

        PropertyDefinition id = null;

        Collection<PropertyDefinition> properties = new ArrayList<>(7);
        Map<Class<?>, DataTypeNormalizer<?>> normalizers = new HashMap<>();

        for (final Method method : typeInterface.getMethods()) {

            String propertyVocab = vocab;
            Vocab methodVocab = method.getAnnotation(Vocab.class);
            if (methodVocab != null) {
                propertyVocab = methodVocab.value();
            }

            String propertyName = method.getName();
            Term methodTerm = method.getAnnotation(Term.class);
            if (methodTerm != null) {
                if (methodTerm.compact()) {
                    propertyName = methodTerm.value();
                }
                if (methodTerm.vocab() != null) {
                    propertyVocab = methodTerm.vocab();
                }
            }

            boolean targetFragment = method.getReturnType().isAnnotationPresent(Fragment.class);

            Literal literal = method.getAnnotation(Literal.class);

            DataTypeNormalizer<?> normalizer = normalizers.get(method.getReturnType());
            ;

            if (literal != null && normalizer == null
                    && DataTypeNormalizer.class.isAssignableFrom(literal.value())) {
                try {
                    normalizer = DataTypeNormalizer.class.cast(literal.value().getDeclaredConstructor().newInstance());
                    normalizers.put(method.getReturnType(), normalizer);

                } catch (InstantiationException | IllegalAccessException | IllegalArgumentException | InvocationTargetException | NoSuchMethodException | SecurityException e) {
                    throw new IllegalStateException(e);
                }
            }

            PropertyDefinition def = new PropertyDefinition(
                    propertyName,
                    propertyVocab,
                    method,
                    targetFragment,
                    normalizer);

            if (method.isAnnotationPresent(Id.class)) {
                id = def;

            } else if (Type.class.isAssignableFrom(method.getReturnType())) {
                type = def;

            } else {
                properties.add(def);
            }
        }

        fragments.put(typeInterface, new TypeDefinition(name, context, id, type, properties, normalizers));
    }

    public JsonObject writeCompact(Object object) {

        Objects.requireNonNull(object);

        final Map<String, JsonValue> fragment = new LinkedHashMap<>(7);

        final Collection<String> context = new LinkedHashSet<>(2);

        PropertyDefinition id = null;
        PropertyDefinition type = null;
        final Collection<String> types = new ArrayList<>(1);

        for (final Class<?> typeInterface : object.getClass().getInterfaces()) {

            TypeDefinition typeDef = fragments.get(typeInterface);

            if (typeDef == null) {
                continue;
            }

            typeDef.context().forEach(context::add);

            if (typeDef.id() != null && id == null) {
                id = typeDef.id();
            }

            if (typeDef.type() != null && type == null) {
                type = typeDef.type();
            }

            if (typeDef.name() != null) {
                types.add(typeDef.name());
            }

            for (final PropertyDefinition property : typeDef.methods()) {

                final JsonValue value = property(property, object, fragment);

                if (value != null) {
                    fragment.put(property.name(), value);
                }
            }
        }

        Map.Entry<String, JsonValue> idEntry = null;

        if (id != null) {
            idEntry = Map.entry(id.name(), property(id, object, fragment));
        }

        Map.Entry<String, JsonValue> typeEntry = null;

        if (type != null) {
            if (types.size() == 1) {
                typeEntry = Map.entry(type.name(), Json.createValue(types.iterator().next()));

            } else if (types.size() > 0) {
                typeEntry = Map.entry(type.name(), Json.createArrayBuilder(types).build());
            }
        }

        return materialize(
                context,
                idEntry,
                typeEntry,
                fragment);
    }

    JsonValue property(PropertyDefinition property, Object object, Map<String, JsonValue> fragment) {
        Object value = property.invoke(object);

        if (value == null) {
            return null;
        }

        if (value instanceof JsonValue jsonValue) {
            return jsonValue;
        }

        if (property.isTargetFragment()) {

        } else {

            DataTypeNormalizer normalizer = property.normalizer();

            if (normalizer == null) {
                // TODO default normalizers
            }

            if (normalizer == null) {
                return Json.createValue(value.toString());
            }
            return Json.createValue(normalizer.normalize(value));
        }
        return null;
    }

    JsonObject materialize(
            Collection<String> context,
            Map.Entry<String, JsonValue> id,
            Map.Entry<String, JsonValue> type,
            Map<String, JsonValue> fragment) {

        JsonObjectBuilder builder = Json.createObjectBuilder();

        if (context.size() == 1) {
            builder.add(JsonLdKeyword.CONTEXT, context.iterator().next());

        } else if (context.size() > 0) {
            builder.add(JsonLdKeyword.CONTEXT, Json.createArrayBuilder(context));
        }

        if (id != null) {
            builder.add(id.getKey(), id.getValue());
        }

        if (type != null) {
            builder.add(type.getKey(), type.getValue());
        }

        for (Map.Entry<String, JsonValue> entries : fragment.entrySet()) {
            builder.add(entries.getKey(), entries.getValue());
        }

        return builder.build();
    }

}
