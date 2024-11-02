package com.apicatalog.linkedtree.fragment;

import java.util.Collection;
import java.util.Map;

import com.apicatalog.linkedtree.LinkedContainer;
import com.apicatalog.linkedtree.LinkedFragment;
import com.apicatalog.linkedtree.LinkedTree;
import com.apicatalog.linkedtree.link.Link;
import com.apicatalog.linkedtree.type.FragmentType;

public record GenericFragment(
        Link id,
        FragmentType type,
        Map<String, LinkedContainer> entries,
        LinkedTree root) implements LinkedFragment {

    @Override
    public Collection<String> terms() {
        return entries.keySet();
    }

    @Override
    public LinkedContainer container(String term) {
        return entries.get(term);
    }
//
//    @Override
//    @SuppressWarnings("unchecked")
//    public <T> T cast(Class<T> clazz) {
//        if (id() != null 
//                && id().target() != null
//                && !Objects.equals(id().target(), this)) {
//            return (T) id().target().cast(clazz);
//        }
//        return (T) this;
//    }
//
//    @Override
//    public Linkable cast() {
//        if (id() != null 
//                && id().target() != null
//                && !Objects.equals(id().target(), this)) {
//            return id().target().cast();
//        }
//        return this;
//    }

    @Override
    public String toString() {
        return "GenericFragment [id=" + id + ", type=" + type + ", entries=" + entries.size() + "]";
    }

    @Override
    public int hashCode() {
        return System.identityHashCode(this);
    }

    @Override
    // containers cannot be compared as instances
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }
        return (obj != null);
    }
}
