package com.apicatalog.ld.lens;

@FunctionalInterface
public interface Lens<T, V> {

    /**
     * Retrieves a value of type V from a target of type T.
     */
    V get(T target);

    /**
     * Returns a new instance of T with the given value set.
     * For mutable structures, this may modify the original.
     * For immutable structures, this should return a modified copy.
     */
    default T set(T target, V value) {
        throw new UnsupportedOperationException("Set operation not supported.");
    }

    /**
     * Composes this lens with another lens to focus deeper into a structure.
     */
    default <U> Lens<T, U> compose(Lens<V, U> inner) {
        return new Lens<>() {
            @Override
            public U get(T target) {
                return inner.get(Lens.this.get(target));
            }

            @Override
            public T set(T target, U value) {
                V outer = Lens.this.get(target);
                V updatedOuter = inner.set(outer, value);
                return Lens.this.set(target, updatedOuter);
            }
        };
    }
}