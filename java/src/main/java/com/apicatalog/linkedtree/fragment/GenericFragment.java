package com.apicatalog.linkedtree.fragment;

import java.util.Collection;
import java.util.Map;

import com.apicatalog.linkedtree.LinkedContainer;
import com.apicatalog.linkedtree.LinkedFragment;
import com.apicatalog.linkedtree.LinkedTree;
import com.apicatalog.linkedtree.link.Link;
import com.apicatalog.linkedtree.type.FragmentType;

public class GenericFragment implements LinkedFragment {

    protected final Link id;
    protected final FragmentType type;
    protected final Map<String, LinkedContainer> entries;
    protected final LinkedTree root;

    public GenericFragment(Link id,
            final FragmentType type,
            final Map<String, LinkedContainer> entries,
            final LinkedTree root) {
        this.id = id;
        this.type = type;
        this.entries = entries;
        this.root = root;
    }

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
    public Link id() {
        return id;
    }

    @Override
    public FragmentType type() {
        return type;
    }

    public Map<String, LinkedContainer> entries() {
        return entries;
    }

    @Override
    public LinkedTree root() {
        return root;
    }
}
