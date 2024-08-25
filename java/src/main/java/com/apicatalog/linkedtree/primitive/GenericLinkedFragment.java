package com.apicatalog.linkedtree.primitive;

import java.util.Collection;
import java.util.Map;

import com.apicatalog.linkedtree.LinkedContainer;
import com.apicatalog.linkedtree.LinkedFragment;
import com.apicatalog.linkedtree.link.Link;
import com.apicatalog.linkedtree.pi.ProcessingInstruction;

public record GenericLinkedFragment(
        Link id,
        Collection<String> type,
        Map<String, LinkedContainer> entries,
        ProcessingInstruction pi) implements LinkedFragment {

    @Override
    public Collection<String> terms() {
        return entries.keySet();
    }

    @Override
    public LinkedContainer property(String term) {
        return entries.get(term);
    }
}