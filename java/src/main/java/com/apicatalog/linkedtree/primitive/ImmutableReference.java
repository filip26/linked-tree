package com.apicatalog.linkedtree.primitive;

import java.util.Collection;
import java.util.Collections;

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
    public LinkedContainer property(String term) {
        return null;
    }

}
