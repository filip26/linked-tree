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

import com.apicatalog.linkedtree.jsonld.JsonLdKeyword;
import com.apicatalog.linkedtree.literal.adapter.DataTypeNormalizer;
import com.apicatalog.linkedtree.orm.Context;
import com.apicatalog.linkedtree.orm.Fragment;

import jakarta.json.Json;
import jakarta.json.JsonObject;
import jakarta.json.JsonObjectBuilder;
import jakarta.json.JsonValue;

public class ObjectJsonLdWriter {

    Map<Class<?>, JsonLdFragmentType> fragments;
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

        Context fragmentContext = typeInterface.getDeclaredAnnotation(Context.class);

        Collection<String> context = Collections.emptySet();

        if (fragmentContext != null) {
            context = Set.of(fragmentContext.value());
        }

        // process type
        if (!fragment.generic()) {

        }

        Collection<JsonLdProperty> properties = new ArrayList<>(7);

        for (Method method : typeInterface.getMethods()) {

            properties.add(new JsonLdProperty(method.getName(), method));

        }

        fragments.put(typeInterface, new JsonLdFragmentType(context, properties));
    }
    
    public JsonObject writeCompact(Object object) {

        Objects.requireNonNull(object);

        Map<String, Object> fragment = new LinkedHashMap<>(7);

        //
        Collection<String> context = new LinkedHashSet<>(3);

//        builder.add(JsonLdKeyword.CONTEXT, context);

        for (final Class<?> type : object.getClass().getInterfaces()) {
System.out.println("X " + type);
            JsonLdFragmentType fragmentType = fragments.get(type);

            if (fragmentType == null) {
                //TODO log?!
                continue;
            }
            
            fragmentType.context().forEach(context::add);

            for (final JsonLdProperty property : fragmentType.methods()) {

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

        return Json.createObjectBuilder(fragment).add(JsonLdKeyword.CONTEXT,
                Json.createArrayBuilder(context)).build();

    }

}
