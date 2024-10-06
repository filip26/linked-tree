package com.apicatalog.linkedtree.orm.mapper;

import java.util.List;
import java.util.Map;

import com.apicatalog.linkedtree.LinkedTree;
import com.apicatalog.linkedtree.adapter.NodeAdapterError;
import com.apicatalog.linkedtree.builder.TreeBuilderError;
import com.apicatalog.linkedtree.jsonld.io.JsonLdTreeReader;
import com.apicatalog.linkedtree.orm.adapter.NativeFragmentAdapter;

import jakarta.json.JsonArray;

public class TreeMapper {

    Map<Class<?>, NativeFragmentAdapter> fragmentAdapters;
    JsonLdTreeReader reader;

    TreeMapper(JsonLdTreeReader reader, Map<Class<?>, NativeFragmentAdapter> fragmentAdapters) {
        this.reader = reader;
        this.fragmentAdapters = fragmentAdapters;
    }

    @SuppressWarnings("unchecked")
    public <T> T get(Class<T> clazz, List<String> context, JsonArray expanded) throws TreeBuilderError, NodeAdapterError {

        LinkedTree tree = reader.read(context, expanded);
        if (tree == null) {
            return null;
        }

        if (tree.type().isAdaptableTo(clazz)) {
            return tree.materialize(clazz);
        }

        if (fragmentAdapters.containsKey(clazz)) {
            return (T) fragmentAdapters.get(clazz).materialize(tree.fragment());
        }
        
        throw new ClassCastException();
    }

}
