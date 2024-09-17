package com.apicatalog.linkedtree.primitive;

import java.util.Collection;
import java.util.Collections;
import java.util.Objects;

import com.apicatalog.linkedtree.LinkedContainer;
import com.apicatalog.linkedtree.LinkedFragment;
import com.apicatalog.linkedtree.LinkedTree;
import com.apicatalog.linkedtree.link.Link;
import com.apicatalog.linkedtree.type.Type;

public record ImmutableReference(
        Link id,
        LinkedTree root
        ) implements LinkedFragment {
    
    @Override
    public Type type() {
        return Type.empty();
    }

    @Override
    public Collection<String> terms() {
        return Collections.emptySet();
    }

    @Override
    public LinkedContainer container(String term) {
        return null;
    }
    
    @Override
    public int hashCode() {
        return Objects.hash(id);
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj == null)
            return false;
        if (getClass() != obj.getClass())
            return false;
        ImmutableReference other = (ImmutableReference) obj;
        return Objects.equals(id, other.id);
    }

    @Override
    public String toString() {
        return "ImmutableReference [id=" + id + "]";
    }

}
