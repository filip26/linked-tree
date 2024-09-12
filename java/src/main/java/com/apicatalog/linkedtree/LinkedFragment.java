package com.apicatalog.linkedtree;

import java.net.URI;
import java.time.Instant;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Objects;
import java.util.function.Function;

import com.apicatalog.linkedtree.adapter.AdapterError;
import com.apicatalog.linkedtree.jsonld.JsonLdKeyword;
import com.apicatalog.linkedtree.lang.LangStringSelector;
import com.apicatalog.linkedtree.lang.LanguageMap;
import com.apicatalog.linkedtree.link.Link;
import com.apicatalog.linkedtree.selector.InvalidSelector;
import com.apicatalog.linkedtree.type.Type;
import com.apicatalog.linkedtree.xsd.XsdDateTime;

public interface LinkedFragment extends LinkedNode {

    /**
     * An optional unique fragment link if an identifier is present. The same
     * {@link Link} can be shared among many fragments enabling composition.
     */
    Link id();

    Type type();

    Collection<String> terms();

    LinkedContainer container(String term);

    @SuppressWarnings("unchecked")
    default <T extends Linkable> T object(String term, Class<T> clazz) throws InvalidSelector, AdapterError {

        Objects.requireNonNull(clazz);

        final LinkedContainer container = container(term);

        if (container == null || container.nodes().isEmpty()) {
            return null;
        }

        if (container.nodes().size() != 1) {
            throw new InvalidSelector(term);
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

            if (node.isLiteral()
                    && clazz.isInstance(node.asLiteral().cast())) {

                return node.asLiteral().cast(clazz);
            }
            throw new InvalidSelector(term);

        } catch (ClassCastException e) {
            throw new InvalidSelector(e, term);
        }
    }

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

    default <T extends Linkable, R> R object(String term, Class<T> clazz, Function<T, R> mapper) throws InvalidSelector {

        Objects.requireNonNull(mapper);

        try {
            T value = object(term, clazz);

            if (value == null) {
                return null;
            }
            return mapper.apply(value);

        } catch (InvalidSelector e) {
            throw e;
        } catch (Exception e) {
            throw new InvalidSelector(e, term);
        }
    }

    default Instant xsdDateTime(String term) throws InvalidSelector {
        return object(
                term,
                XsdDateTime.class,
                XsdDateTime::datetime);
    }

    default String lexeme(String term) throws InvalidSelector {
        return object(
                term,
                LinkedLiteral.class,
                LinkedLiteral::lexicalValue);
    }

    default LinkedFragment fragment(String term) throws InvalidSelector {
        return object(
                term,
                LinkedFragment.class,
                Function.identity());
    }

    default URI uri() throws InvalidSelector {
        if (id() == null) {
            return null;
        }
        try {
            final String uri = id().uri();
            if (uri != null) {
                return URI.create(uri);
            }
        } catch (IllegalArgumentException e) {

        }
        throw new InvalidSelector(JsonLdKeyword.ID);
    }

    default URI uri(String term) throws InvalidSelector, AdapterError {
        try {

            LinkedFragment node = object(term, LinkedFragment.class);

            if (node != null) {

                final String uri = node.asFragment().id().uri();
                if (uri != null) {
                    return URI.create(uri);
                }
            }

            throw new InvalidSelector(term);

        } catch (IllegalArgumentException e) {
            throw new InvalidSelector(term);
        }
    }

    default LangStringSelector langMap(String term) throws InvalidSelector {
        final LinkedContainer container = container(term);
        if (container != null) {
            return LanguageMap.of(container);
        }
        return null;
    }

    default <T> Collection<T> collection(
            String term,
            Class<T> clazz) throws InvalidSelector {
        return collection(term, clazz, null);
    }

    default <T> Collection<T> collection(
            String term,
            Class<T> clazz,
            Function<LinkedNode, T> unmapped) throws InvalidSelector {

        final LinkedContainer container = container(term);

        if (container == null || container.nodes().isEmpty()) {
            return Collections.emptyList();
        }

        try {
            var collection = new ArrayList<T>(container.nodes().size());

            for (final LinkedNode node : container) {
                
                if (node.isFragment() && node.asFragment().type().isAdaptableTo(clazz)) {
                    collection.add(node.asFragment().type().materialize(clazz));

                } else if (node.isLiteral() && clazz.isInstance(node.asLiteral().cast())) {
                    collection.add(node.asLiteral().cast(clazz));

                } else if (unmapped != null) {
                    collection.add(unmapped.apply(node));

                } else {
                    throw new InvalidSelector(term);
                }
            }

            return collection;

        } catch (AdapterError e) {
            throw new InvalidSelector(e, term);
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
