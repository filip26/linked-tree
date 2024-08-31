package com.apicatalog.linkedtree.primitive;

import java.util.Collection;
import java.util.Collections;

import com.apicatalog.linkedtree.Link;
import com.apicatalog.linkedtree.LinkedContainer;
import com.apicatalog.linkedtree.LinkedFragment;

public record ImmutableReference(Link id) implements LinkedFragment {

    public static final ImmutableReference of(Link id) {
        return new ImmutableReference(id);
    }

    @Override
    public Collection<String> type() {
        return Collections.emptySet();
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
