package com.apicatalog.linkedtree;

import java.net.URI;
import java.time.Instant;
import java.util.Collection;
import java.util.Objects;
import java.util.function.Function;

import com.apicatalog.linkedtree.lang.LangStringSelector;
import com.apicatalog.linkedtree.lang.LanguageMap;
import com.apicatalog.linkedtree.link.Link;
import com.apicatalog.linkedtree.selector.InvalidSelector;
import com.apicatalog.linkedtree.type.Type;
import com.apicatalog.linkedtree.type.TypeAdapterError;
import com.apicatalog.linkedtree.xsd.XsdDateTime;

public interface LinkedFragment extends LinkedNode {

    /**
     * An optional unique fragment link if an identifier is present. The same
     * {@link Link} can be shared among many fragments enabling composition.
     */
    Link id();

    Type type();

    Collection<String> terms();

    LinkedContainer property(String term);

    @SuppressWarnings("unchecked")
    default <T extends Linkable> T single(String term, Class<T> clazz) throws InvalidSelector, TypeAdapterError {

        Objects.requireNonNull(clazz);

        final LinkedContainer container = property(term);

        if (container == null || container.nodes().isEmpty()) {
            return null;
        }

        if (container.nodes().size() != 1) {
            throw new InvalidSelector(term);
        }

        try {
            final LinkedNode node = container.single();

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

    default <T extends Linkable, R> R single(String term, Class<T> clazz, Function<T, R> mapper) throws InvalidSelector {

        Objects.requireNonNull(mapper);

        try {
            T value = single(term, clazz);

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
        return single(
                term,
                XsdDateTime.class,
                XsdDateTime::datetime);
    }

    default String lexeme(String term) throws InvalidSelector {
        return single(
                term,
                LinkedLiteral.class,
                LinkedLiteral::lexicalValue);
    }

    default LinkedFragment singleFragment(String term) throws InvalidSelector {
        return single(
                term,
                LinkedFragment.class,
                Function.identity());
    }

//
//    public static URI id(Link link) throws DocumentError {
//        if (link == null) {
//            return null;
//        }
//        try {
//            final String uri = link.uri();
//            if (uri != null) {
//                return URI.create(uri);
//            }
//        } catch (IllegalArgumentException e) {
//
//        }
//        throw new DocumentError(ErrorType.Invalid, Keywords.ID);
//    }

    default URI id(String term) throws InvalidSelector, TypeAdapterError {
        try {

            LinkedFragment node = single(term, LinkedFragment.class);

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
        final LinkedContainer container = property(term);
        if (container != null) {
            return LanguageMap.of(container);
        }
        return null;
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
