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
import jakarta.json.JsonArrayBuilder;
import jakarta.json.JsonObject;
import jakarta.json.JsonObjectBuilder;
import jakarta.json.JsonValue;
import jakarta.json.JsonValue.ValueType;

public class JsonLdObjectWriter {

    Map<Class<?>, TypeDefinition> fragments;
    Map<Class<?>, DataTypeNormalizer<?>> datatypes;

    public JsonLdObjectWriter() {
        this.fragments = new HashMap<>();
        this.datatypes = new HashMap<>();
    }

    public JsonLdObjectWriter scan(Class<?> type) {

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

            Literal literal = method.getAnnotation(Literal.class);

            DataTypeNormalizer<?> normalizer = normalizers.get(method.getReturnType());

            if (literal != null && normalizer == null
                    && DataTypeNormalizer.class.isAssignableFrom(literal.value())) {
                try {
                    normalizer = DataTypeNormalizer.class.cast(literal.value().getDeclaredConstructor().newInstance());
                    normalizers.put(method.getReturnType(), normalizer);

                } catch (InstantiationException | IllegalAccessException | IllegalArgumentException | InvocationTargetException | NoSuchMethodException | SecurityException e) {
                    throw new IllegalStateException(e);
                }
            }

            final PropertyDefinition def = PropertyDefinition.of(
                    propertyName,
                    propertyVocab,
                    method,
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

    public JsonObject compacted(Object object) {
        Objects.requireNonNull(object);

        return compacted(new LinkedHashSet<>(2), object, true);
    }

    JsonObject compacted(final Collection<String> context, final Object object, boolean attachContext) {

        final Map<String, JsonValue> fragment = new LinkedHashMap<>(7);

        PropertyDefinition id = null;
        PropertyDefinition type = null;

        final Collection<String> types = new LinkedHashSet<>(1);

        for (final Class<?> typeInterface : object.getClass().getInterfaces()) {

            TypeDefinition typeDef = fragments.get(typeInterface);

            if (typeDef == null) {
                continue;
            }

            //TODO better, use ContextReducer, detect and resolve
            if (attachContext) {
                typeDef.context().forEach(context::add);
            }

            if (typeDef.id() != null && id == null) {
                id = typeDef.id();
            }

            if (typeDef.type() != null && type == null) {
                type = typeDef.type();
            }

            if (typeDef.name() != null) {
                types.add(typeDef.name());
            }

            for (final PropertyDefinition propertyDef : typeDef.methods()) {

                final JsonValue value = property(context, propertyDef, object, fragment);

                if (value != null) {
                    fragment.put(propertyDef.name(), value);
                }
            }
        }

        Map.Entry<String, JsonValue> idEntry = null;

        if (id != null) {
            idEntry = Map.entry(id.name(), property(context, id, object, fragment));
        }

        Map.Entry<String, JsonValue> typeEntry = null;

        if (type != null) {

            if (types.isEmpty()) {
                Type objectTypes = (Type) type.invoke(object);
                if (objectTypes != null) {
                    objectTypes.forEach(types::add);
                }
            }

            if (types.size() == 1) {
                typeEntry = Map.entry(type.name(), Json.createValue(types.iterator().next()));

            } else if (types.size() > 0) {
                typeEntry = Map.entry(type.name(), Json.createArrayBuilder(types).build());
            }
        }

        return materialize(
                attachContext ? context : Collections.emptyList(),
                idEntry,
                typeEntry,
                fragment);
    }

    JsonValue property(Collection<String> context, PropertyDefinition propertyDef, Object object, Map<String, JsonValue> fragment) {

        Object value = propertyDef.invoke(object);

        if (value == null) {
            return null;
        }

        if (value instanceof JsonValue jsonValue) {
            if (ValueType.ARRAY.equals(jsonValue.getValueType())
                    && jsonValue.asJsonArray().isEmpty()) {
                return null;
            }
            if (ValueType.OBJECT.equals(jsonValue.getValueType())
                    && jsonValue.asJsonObject().isEmpty()) {
                return null;
            }
            return jsonValue;
        }

        if (value instanceof Collection collection) {
            if (collection.isEmpty()) {
                return null;
            }
            if (collection.size() > 1) {
                JsonArrayBuilder array = Json.createArrayBuilder();

                for (Object item : collection) {
                    array.add(item(context, propertyDef, item));
                }

                return array.build();
            }
            return value(context, propertyDef, collection.iterator().next());
        }

        return value(context, propertyDef, value);
    }

    JsonValue item(Collection<String> context, PropertyDefinition propertyDef, Object object) {
        if (object == null) {
            return JsonValue.NULL;
        }
        return value(context, propertyDef, object);
    }

    JsonValue value(Collection<String> context, PropertyDefinition propertyDef, Object object) {

        if (propertyDef.isTargetFragment()) {
            return compacted(context, object, false);
        }

        DataTypeNormalizer normalizer = propertyDef.normalizer();

        if (normalizer == null) {
            // TODO default normalizers
        }

        if (normalizer == null) {
            return Json.createValue(object.toString());
        }

        return Json.createValue(normalizer.normalize(object));
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
