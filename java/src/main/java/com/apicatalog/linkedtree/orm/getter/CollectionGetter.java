package com.apicatalog.linkedtree.orm.getter;

import java.util.Collection;
import java.util.Collections;
import java.util.LinkedHashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;

import com.apicatalog.linkedtree.LinkedFragment;
import com.apicatalog.linkedtree.LinkedNode;
import com.apicatalog.linkedtree.adapter.NodeAdapter;
import com.apicatalog.linkedtree.adapter.NodeAdapterError;
import com.apicatalog.linkedtree.orm.adapter.FragmentProxy;

public class CollectionGetter implements Getter {

    final String term;
    final Class<?> collectionType;
    final Class<Object> componentType;
    final NodeAdapter<LinkedNode, Object> adapter;
    
    CollectionGetter(
            String term,
            Class<?> collectionType,
            Class<Object> componentType,
            NodeAdapter<LinkedNode, Object> adapter
            ) {
        this.term = term;
        this.collectionType = collectionType;
        this.componentType = componentType;
        this.adapter = adapter;
    }
    
    @SuppressWarnings({ "unchecked", "rawtypes" })
    public static CollectionGetter of(String term, Class<?> collectionType, Class<?> componentType, FragmentProxy adapter) {
        return new CollectionGetter(term, collectionType, (Class<Object>)componentType, (NodeAdapter)adapter);
    }

    @SuppressWarnings({ "unchecked", "rawtypes" })
    public static CollectionGetter of(String term, Class<?> collectionType, Class<?> componentType, NodeAdapter<LinkedNode, ?> adapter) {
        return new CollectionGetter(term, collectionType, (Class<Object>)componentType, (NodeAdapter)adapter);
    }

    @Override
    public Object get(LinkedFragment source) throws NodeAdapterError {
        
        Collection<Object> coll = source.collection(term, componentType, adapter);
        
        if (collectionType.isInstance(coll)) {
            return coll;
        }
        
        if (collectionType.isAssignableFrom(Set.class)) {
            return Collections.unmodifiableSet(new LinkedHashSet<>(coll));
        }

        if (collectionType.isAssignableFrom(List.class)) {
            return Collections.unmodifiableList(new LinkedList<>(coll));
        }

        return coll;
    }

}
