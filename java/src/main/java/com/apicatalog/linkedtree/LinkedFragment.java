package com.apicatalog.linkedtree;

import java.net.URI;
import java.time.Instant;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Objects;
import java.util.function.Function;

import com.apicatalog.linkedtree.adapter.NodeAdapter;
import com.apicatalog.linkedtree.adapter.NodeAdapterError;
import com.apicatalog.linkedtree.fragment.FragmentPropertyError;
import com.apicatalog.linkedtree.jsonld.JsonLdKeyword;
import com.apicatalog.linkedtree.lang.LangStringSelector;
import com.apicatalog.linkedtree.lang.LanguageMap;
import com.apicatalog.linkedtree.link.Link;
import com.apicatalog.linkedtree.type.FragmentType;
import com.apicatalog.linkedtree.xsd.XsdDateTime;

public interface LinkedFragment extends LinkedNode {

    /**
     * An optional unique fragment link if an identifier is present. The same
     * {@link Link} can be shared among many fragments enabling composition.
     */
    Link id();

    FragmentType type();

    Collection<String> terms();

    LinkedContainer container(String term);

    @SuppressWarnings("unchecked")
    default <T> T materialize(String term, Class<T> clazz) throws NodeAdapterError {

        Objects.requireNonNull(clazz);

        final LinkedContainer container = container(term);

        if (container == null || container.nodes().isEmpty()) {
            return null;
        }

        if (container.nodes().size() != 1) {
            throw new FragmentPropertyError("Expected single object but got a container of " + container.nodes().size() + " items.", term);
        }

        try {
            final LinkedNode node = container.node();

            if (node == null) {
                return null;
            }

            if (clazz.isInstance(node)) {
                return (T) node;
            }

            if (node.isFragment()
                    && node.asFragment().type().isAdaptableTo(clazz)) {
                return node.asFragment().type().materialize(clazz);
            }

            if (node.isLiteral() && clazz.isInstance(node)) {
                return clazz.cast(node.asLiteral());
            }

        } catch (ClassCastException e) {
            throw new FragmentPropertyError(e, term);
        }

        throw new FragmentPropertyError("Value cannot be cast to " + clazz, term);
    }

    default LinkedNode node(String term) throws FragmentPropertyError {
        final LinkedContainer container = container(term);

        if (container == null || container.nodes().isEmpty()) {
            return null;
        }

        if (container.nodes().size() != 1) {
            throw new FragmentPropertyError("Expected single object but got a container of " + container.nodes().size() + " items.", term);
        }
        return container.node();
    }

    /**
     * A {@link LinkedTree} instance to which the {@link LinkedNode} belongs to.
     * 
     * Please note a tree instance can have a root if is a child node of another
     * tree instance.
     * 
     * @return an instance or <code>null</code> if the node is a root
     */
    LinkedTree root();

//    default <R> R single(String term, LinkableMapper<R> mapper) throws DocumentError {
//
//        Objects.requireNonNull(term);
//
//        final LinkedContainer container = properties.getOrDefault(term.uri(), LinkedContainer.EMPTY);
//
//        if (container.nodes().isEmpty()) {
//            return null;
//        }
//
//        if (container.nodes().size() != 1) {
//            throw new DocumentError(ErrorType.Invalid, term);
//        }
//
//        try {
//            final LinkedNode node = container.single();
//
//            if (node == null) {
//                return null;
//            }
//
//            Linkable value = null;
//
//            if (node.isFragment()) {
//                value = node.asFragment().cast();
//
//            } else if (node.isLiteral()) {
//                value = node.asLiteral();
//
//            } else {
//                throw new DocumentError(ErrorType.Invalid, term);
//            }
//
//            return mapper.map(value);
//
//        } catch (DocumentError e) {
//            throw e;
//        } catch (Exception e) {
//            throw new DocumentError(e, ErrorType.Invalid, term);
//        }
//    }

    @SuppressWarnings("unchecked")
    default <R> R fragment(String term, Class<R> clazz, NodeAdapter<LinkedNode, R> mapper) throws NodeAdapterError {

//        try {
        LinkedFragment node = fragment(term);

        if (node == null) {
            return null;
        }

        if (clazz.isInstance(node)) {
            return (R) node;
        }

        if (node.type().isAdaptableTo(clazz)) {
            return node.type().materialize(clazz);
        }

        if (mapper != null) {
            return mapper.materialize(node);
        }

        throw new NodeAdapterError();
//        } catch (InvalidSelector e) {
//            throw e;
//        } catch (Exception e) {
//            throw new InvalidSelector(e, term);
//        }
    }

    default <T> T literal(String term, Class<T> clazz) throws FragmentPropertyError {
        return literal(term, clazz, Function.identity());
    }

    default <T, R> R literal(String term, Class<T> clazz, Function<T, R> mapper) throws FragmentPropertyError {
        return literal(term, clazz, mapper, null);
    }

    default <T, R> R literal(String term, Class<T> clazz, Function<T, R> mapper, R defaultValue) throws FragmentPropertyError {

        Objects.requireNonNull(mapper);

        try {
            T value = materialize(term, clazz);

            if (value == null) {
                return defaultValue;
            }
            return mapper.apply(value);

        } catch (FragmentPropertyError e) {
            throw e;

        } catch (Exception e) {
            throw new FragmentPropertyError(e, term);
        }
    }

    default Instant xsdDateTime(String term) throws NodeAdapterError {
        return literal(
                term,
                XsdDateTime.class,
                XsdDateTime::datetime);
    }

    default String lexicalValue(String term) throws NodeAdapterError {
        return literal(
                term,
                LinkedLiteral.class,
                LinkedLiteral::lexicalValue);
    }

    default LinkedFragment fragment(String term) throws NodeAdapterError {
        return literal(
                term,
                LinkedFragment.class,
                Function.identity());
    }

    default URI uri() throws NodeAdapterError {
        if (id() == null) {
            return null;
        }
        try {
            final String uri = id().uri();
            if (uri != null) {
                return URI.create(uri);
            }
            return null;

        } catch (IllegalArgumentException e) {
            throw new FragmentPropertyError(e, JsonLdKeyword.ID);
        }
    }

    default URI uri(String term) throws NodeAdapterError {
        try {

            LinkedFragment node = materialize(term, LinkedFragment.class);

            if (node != null) {

                final String uri = node.asFragment().id().uri();
                if (uri != null) {
                    return URI.create(uri);
                }
            }
            return null;

        } catch (IllegalArgumentException e) {
            throw new FragmentPropertyError(e, term);
        }
    }

    default LangStringSelector languageMap(String term) throws NodeAdapterError {
        final LinkedContainer container = container(term);
        if (container != null) {
            try {
                return LanguageMap.of(container);
            } catch (IllegalArgumentException e) {
                throw new FragmentPropertyError(e, term);
            }
        }
        return LangStringSelector.empty();
    }

    default <T> Collection<T> collection(
            String term,
            Class<T> clazz) throws NodeAdapterError {
        return collection(term, clazz, null);
    }

    @SuppressWarnings("unchecked")
    default <T> Collection<T> collection(
            String term,
            Class<T> clazz,
            NodeAdapter<LinkedNode, T> unmapped) throws NodeAdapterError {

        final LinkedContainer container = container(term);

        if (container == null || container.nodes().isEmpty()) {
            return Collections.emptyList();
        }

        try {
            var collection = new ArrayList<T>(container.nodes().size());

            for (final LinkedNode node : container) {

                if (clazz.isInstance(node)) {
                    collection.add((T) node);

                } else if (node.isFragment()
                        && node.asFragment().id() != null
                        && node.asFragment().id().target().type().isAdaptableTo(clazz)) {
                    collection.add(node.asFragment().id().target().type().materialize(clazz));

                } else if (node.isFragment() && node.asFragment().type().isAdaptableTo(clazz)) {
                    collection.add(node.asFragment().type().materialize(clazz));

                } else if (node.isLiteral() && clazz.isInstance(node.asLiteral())) {
                    collection.add(clazz.cast(node.asLiteral()));

                } else if (unmapped != null) {
                    if (node.isFragment()
                            && node.asFragment().id() != null) {
                        collection.add(unmapped.materialize(node.asFragment().id().target()));
                    } else {
                        collection.add(unmapped.materialize(node));
                    }
                } else {
                    throw new FragmentPropertyError("Cannot cast an item to " + clazz, term);
                }
            }

            return collection;

        } catch (FragmentPropertyError e) {
            throw e;

        } catch (NodeAdapterError e) {
            throw new FragmentPropertyError(e, term);
        }
    }

    @Override
    default boolean isFragment() {
        return true;
    }

    @Override
    default LinkedFragment asFragment() {
        return this;
    }
}
