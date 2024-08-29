package com.apicatalog.linkedtree.primitive;

import java.util.Collection;
import java.util.Collections;

import com.apicatalog.linkedtree.LinkedContainer;
import com.apicatalog.linkedtree.LinkedFragment;
import com.apicatalog.linkedtree.link.Link;

public record Reference(Link id) implements LinkedFragment {

    public static final Reference of(Link id) {
        return new Reference(id);
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
