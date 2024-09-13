package com.apicatalog.linkedtree.fragment;

import java.util.Collection;
import java.util.Map;
import java.util.Objects;

import com.apicatalog.linkedtree.LinkedContainer;
import com.apicatalog.linkedtree.LinkedFragment;
import com.apicatalog.linkedtree.LinkedTree;
import com.apicatalog.linkedtree.link.Link;
import com.apicatalog.linkedtree.type.Type;

public record GenericFragment(
        Link id,
        Type type,
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
        return "GenericFragment [id=" + id + ", type=" + type + ", entries=" + entries + "]";
    }

    @Override
    public int hashCode() {
        return Objects.hash(entries, id, type);
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj == null)
            return false;
        if (getClass() != obj.getClass())
            return false;
        GenericFragment other = (GenericFragment) obj;
        return Objects.equals(entries, other.entries) && Objects.equals(id, other.id) && Objects.equals(type, other.type);
    }

}
