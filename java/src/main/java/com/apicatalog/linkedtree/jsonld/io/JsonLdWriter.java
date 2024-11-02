package com.apicatalog.linkedtree.jsonld.io;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.net.URI;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.logging.Logger;

import com.apicatalog.linkedtree.Linkable;
import com.apicatalog.linkedtree.def.PropertyDefinition;
import com.apicatalog.linkedtree.def.TypeDefinition;
import com.apicatalog.linkedtree.jsonld.JsonLdKeyword;
import com.apicatalog.linkedtree.lang.LanguageMap;
import com.apicatalog.linkedtree.literal.adapter.DataTypeNormalizer;
import com.apicatalog.linkedtree.orm.Context;
import com.apicatalog.linkedtree.orm.Fragment;
import com.apicatalog.linkedtree.orm.Id;
import com.apicatalog.linkedtree.orm.Literal;
import com.apicatalog.linkedtree.orm.Term;
import com.apicatalog.linkedtree.orm.Vocab;
import com.apicatalog.linkedtree.orm.context.ContextReducer;
import com.apicatalog.linkedtree.orm.getter.GetterMethod;
import com.apicatalog.linkedtree.type.FragmentType;

import jakarta.json.Json;
import jakarta.json.JsonArrayBuilder;
import jakarta.json.JsonObject;
import jakarta.json.JsonObjectBuilder;
import jakarta.json.JsonValue;
import jakarta.json.JsonValue.ValueType;

public class JsonLdWriter {

    private static final Logger LOGGER = Logger.getLogger(JsonLdWriter.class.getName());

    ContextReducer contextReducer;

    Map<Class<?>, TypeDefinition> fragments;
    Map<Class<?>, DataTypeNormalizer<?>> datatypes;

    public JsonLdWriter() {
        this.contextReducer = new ContextReducer();
        this.fragments = new HashMap<>();
        this.datatypes = new HashMap<>();
    }

    public ContextReducer contextReducer() {
        return contextReducer;
    }

    public JsonLdWriter scan(Class<?> type) {

        Objects.requireNonNull(type);

        if (!type.isInterface()) {
            throw new IllegalArgumentException();
        }

        scanInterface(type);

        return this;
    }

    public JsonLdWriter context(
            String id,
            int position,
            Collection<String> includes) {
        contextReducer.define(id, position, includes);
        return this;
    }

    public JsonLdWriter context(
            String id,
            Collection<String> includes) {
        contextReducer.define(id, includes);
        return this;
    }

    void scanInterface(Class<?> typeInterface) {
        Fragment fragment = typeInterface.getDeclaredAnnotation(Fragment.class);
        if (fragment == null) {
            // TODO log
            return;
        }

        final Collection<String> context = new LinkedHashSet<String>(2);
        final Collection<String> type = new LinkedHashSet<String>(2);

        scanHierarchy(typeInterface, context, type);

        String vocab = null;
        Vocab fragmentVocab = typeInterface.getAnnotation(Vocab.class);
        if (fragmentVocab != null) {
            vocab = fragmentVocab.value();
        }

        PropertyDefinition idMethod = null;
        PropertyDefinition typeMethod = null;

        Collection<PropertyDefinition> properties = new ArrayList<>(7);
        Map<Class<?>, DataTypeNormalizer<?>> normalizers = new HashMap<>();

        for (final Method method : GetterMethod.filter(typeInterface, true)) {

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
                idMethod = def;

            } else if (method.isAnnotationPresent(com.apicatalog.linkedtree.orm.Type.class)
                    || FragmentType.class.isAssignableFrom(method.getReturnType())) {
                typeMethod = def;

            } else {
                properties.add(def);
            }
        }

        fragments.put(typeInterface, new TypeDefinition(
                type,
                context,
                idMethod,
                typeMethod,
                properties,
                normalizers));
    }

    void scanHierarchy(Class<?> typeInterface, Collection<String> context, Collection<String> type) {
        if (typeInterface == null) {
            return;
        }

        if (typeInterface.getInterfaces() != null) {
            for (Class<?> superType : typeInterface.getInterfaces()) {
                scanHierarchy(superType, context, type);
            }
        }

        Fragment fragment = typeInterface.getDeclaredAnnotation(Fragment.class);

        if (fragment != null) {

            Context fragmentContext = typeInterface.getAnnotation(Context.class);
            if (fragmentContext != null) {
                if (fragmentContext.override()) {
                    context.clear();
                }
                for (String ctx : fragmentContext.value()) {
                    context.add(ctx);
                }
            }

            // process type
            if (!fragment.generic()) {
                String name = typeInterface.getSimpleName();

                Term fragmentTerm = typeInterface.getAnnotation(Term.class);

                if (fragmentTerm != null) {
                    name = fragmentTerm.value();
                }
                type.add(name);
            }
        }
    }

    public JsonObject compacted(Object object) {
        Objects.requireNonNull(object);

        return compacted(new LinkedHashSet<>(2), object, true);
    }

    JsonObject compacted(final Collection<String> context, final Object object, boolean attachContext) {

        final Map<String, JsonValue> fragment = new LinkedHashMap<>(7);

        PropertyDefinition id = null;
        PropertyDefinition type = null;

        Collection<String> types = null;

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
                types = typeDef.types(); // TODO
            }

            for (final PropertyDefinition propertyDef : typeDef.methods()) {

                final JsonValue value = property(context, propertyDef, object, fragment);

                if (value != null) {
                    fragment.put(propertyDef.name(), value);
                }
            }
        }

        if (id == null && type == null && fragment.isEmpty()) {
            // fallback
            if (object instanceof Linkable linkable) {
                return JsonLdTreeWriter.fragment(linkable.ld().asFragment());
            }
            // TODO
            return null;
        }

        Map.Entry<String, JsonValue> idEntry = null;

        if (id != null) {
            idEntry = Map.entry(id.name(), property(context, id, object, fragment));
        }

        Map.Entry<String, JsonValue> typeEntry = null;

        if (type != null) {
            if (types == null || types.isEmpty()) {

                Object objectTypes = type.invoke(object);

                if (objectTypes instanceof FragmentType fragmentType) {
                    types = fragmentType.stream().toList();

                } else if (objectTypes instanceof String stringType) {
                    types = List.of(stringType);

                } else if (objectTypes instanceof URI uriType) {
                    types = List.of(uriType.toString());
                    
                } else if (objectTypes instanceof Collection fragmentTypes) {
                    Class<?> typeClass = fragmentTypes.getClass().getComponentType();
                    
                    if (typeClass.isAssignableFrom(String.class)) {
                        types = ((Collection<String>)fragmentTypes);
                        
                    } else if (typeClass.isAssignableFrom(URI.class)) {
                        types = ((Collection<URI>)fragmentTypes).stream().map(URI::toString).toList();
                    }
                }
            }

            if (types != null) {
                if (types.size() == 1) {
                    typeEntry = Map.entry(type.name(), Json.createValue(types.iterator().next()));

                } else if (types.size() > 0) {
                    typeEntry = Map.entry(type.name(), Json.createArrayBuilder(types).build());
                }
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

        if (value instanceof LanguageMap langMap && langMap.size() > 0) {

            if (langMap.size() == 1) {
                return Json.createValue(langMap.first().lexicalValue());
            }

            final JsonArrayBuilder values = Json.createArrayBuilder();

            langMap.values().stream()
                    .map(langString -> {
                        JsonObjectBuilder builder = Json.createObjectBuilder()
                                .add(JsonLdKeyword.VALUE, langString.lexicalValue());

                        if (langString.language() != null) {
                            builder.add(JsonLdKeyword.LANGUAGE, langString.language());
                        }
                        if (langString.direction() != null) {
                            builder.add(JsonLdKeyword.DIRECTION, langString.direction().toString().toLowerCase());
                        }

                        return builder;
                    })
                    .forEach(values::add);

            return values.build();
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

        if (normalizer != null) {
            return Json.createValue(normalizer.normalize(object));

        }

        if (object instanceof Linkable linkable) {
            return JsonLdTreeWriter.node(linkable.ld());
        }

        // TODO default normalizers
        return Json.createValue(object.toString());
    }

    JsonObject materialize(
            Collection<String> context,
            Map.Entry<String, JsonValue> id,
            Map.Entry<String, JsonValue> type,
            Map<String, JsonValue> fragment) {

        JsonObjectBuilder builder = Json.createObjectBuilder();

        Collection<String> reduced = contextReducer.reduce(context);

        if (reduced.size() == 1) {
            builder.add(JsonLdKeyword.CONTEXT, context.iterator().next());

        } else if (reduced.size() > 1) {
            builder.add(JsonLdKeyword.CONTEXT, Json.createArrayBuilder(reduced));
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
