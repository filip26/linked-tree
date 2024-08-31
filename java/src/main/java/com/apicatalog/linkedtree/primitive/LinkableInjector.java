package com.apicatalog.linkedtree.primitive;

import java.util.function.Consumer;
import java.util.function.Supplier;

import com.apicatalog.linkedtree.Linkable;

public class LinkableInjector<T extends Linkable> implements Supplier<T>, Consumer<T> {

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
