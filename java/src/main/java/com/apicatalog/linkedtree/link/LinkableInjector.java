package com.apicatalog.linkedtree.link;

import java.util.function.Consumer;
import java.util.function.Supplier;

import com.apicatalog.linkedtree.LinkedData;

public class LinkableInjector<T extends LinkedData> implements Supplier<T>, Consumer<T> {

    protected T linkable;

    @Override
    public T get() {
        return linkable;
    }

    @Override
    public void accept(T linkable) {
        this.linkable = linkable;
    }
}
