package com.apicatalog.linkedtree.primitive;

import java.util.Collection;
import java.util.Collections;

import com.apicatalog.linkedtree.LinkedContainer;
import com.apicatalog.linkedtree.LinkedFragment;
import com.apicatalog.linkedtree.link.Link;

public record Reference(Link link) implements LinkedFragment {

    public static final Reference of(Link link) {
        return new Reference(link);
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
