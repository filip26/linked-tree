package com.apicatalog.linkedtree.primitive;

import com.apicatalog.linkedtree.LinkedContainer;
import com.apicatalog.linkedtree.LinkedFragment;
import com.apicatalog.linkedtree.LinkedNode;

public record NodeIndex(
        LinkedNode parent,
        Type type,
        int index,
        String term) {

    enum Type {
        ContainerIndex,
        FragmentIndex
    }

    public static NodeIndex container(LinkedContainer container, int index) {
        return new NodeIndex(container, Type.ContainerIndex, index, null);
    }

    public static NodeIndex fragment(LinkedFragment fragment, String term) {
        return new NodeIndex(fragment, Type.ContainerIndex, -1, term);
    }

}
