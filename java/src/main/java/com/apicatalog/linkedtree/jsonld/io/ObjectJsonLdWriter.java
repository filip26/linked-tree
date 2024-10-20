package com.apicatalog.linkedtree.jsonld.io;

import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Arrays;
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
import com.apicatalog.linkedtree.orm.Term;
import com.apicatalog.linkedtree.orm.Vocab;

import jakarta.json.Json;
import jakarta.json.JsonObject;
import jakarta.json.JsonObjectBuilder;

public class ObjectJsonLdWriter {

    Map<Class<?>, TypeDefinition> fragments;
    Map<Class<?>, DataTypeNormalizer> datatypes;

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

//        for (Class<?> typeInterface : type.getInterfaces()) {
//            scanInterface(typeInterface);
//
//        }
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

        // process type
        if (!fragment.generic()) {

        }

        String vocab = null;
        Vocab fragmentVocab = typeInterface.getAnnotation(Vocab.class);
        if (fragmentVocab != null) {
            vocab = fragmentVocab.value();
        }

        PropertyDefinition id = null;
        Collection<PropertyDefinition> properties = new ArrayList<>(7);

        for (final Method method : typeInterface.getMethods()) {

            String propertyName = method.getName();
            Term methodTerm = method.getAnnotation(Term.class);
            if (methodTerm != null) {
                propertyName = methodTerm.value();
            }

            String propertyVocab = vocab;
            Vocab methodVocab = method.getAnnotation(Vocab.class);
            if (methodVocab != null) {
                propertyVocab = methodVocab.value();
            }

            Arrays.stream(method.getAnnotations())
                    .forEach(System.out::println);

            if (method.isAnnotationPresent(Id.class)) {
                id = new PropertyDefinition(propertyName, propertyVocab, method);
                
            } else {
                properties.add(new PropertyDefinition(propertyName, propertyVocab, method));
            }
        }

        fragments.put(typeInterface, new TypeDefinition(context, id, properties));
    }

    public JsonObject writeCompact(Object object) {

        Objects.requireNonNull(object);

        Map<String, Object> fragment = new LinkedHashMap<>(7);

        //
        Collection<String> context = new LinkedHashSet<>(3);

//        builder.add(JsonLdKeyword.CONTEXT, context);

        for (final Class<?> type : object.getClass().getInterfaces()) {
            System.out.println("X " + type);
            TypeDefinition fragmentType = fragments.get(type);

            if (fragmentType == null) {
                // TODO log?!
                continue;
            }

            fragmentType.context().forEach(context::add);

            for (final PropertyDefinition property : fragmentType.methods()) {

                Object value = property.invoke(object);

                if (value != null) {
                    fragment.put(property.name(), value.toString());
                }
//
//                
//
//                property.invoke(object);
//                
//                
//                
            }

            System.out.println(type);
            Arrays.stream(type.getDeclaredAnnotations())
                    .forEach(System.out::println);
            System.out.println("--");
//            Arrays.stream(type.getAnnotations())
//            .forEach(System.out::println);

        }

        return materialize(context, fragment);

    }
    
    JsonObject materialize(Collection<String> context, Map<String, Object> fragment) {
        
        JsonObjectBuilder builder = Json.createObjectBuilder();

        if (context.size() == 1) {
            builder.add(JsonLdKeyword.CONTEXT, context.iterator().next());
            
        } else if (context.size() > 0) {
            builder.add(JsonLdKeyword.CONTEXT, Json.createArrayBuilder(context));
        }
        
        for (Map.Entry<String, Object> entries : fragment.entrySet()) {
            builder.add(entries.getKey(), entries.getKey());
        }
        
        return builder.build();        
    }

}
