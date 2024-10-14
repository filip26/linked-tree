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
import com.apicatalog.linkedtree.jsonld.JsonLdKeyword;
import com.apicatalog.linkedtree.lang.LangStringSelector;
import com.apicatalog.linkedtree.lang.LanguageMap;
import com.apicatalog.linkedtree.link.Link;
import com.apicatalog.linkedtree.selector.InvalidSelector;
import com.apicatalog.linkedtree.type.Type;
import com.apicatalog.linkedtree.writer.DebugNodeWriter;
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
    default <T extends Linkable> T materialize(String term, Class<T> clazz) throws InvalidSelector, NodeAdapterError {

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
            
            DebugNodeWriter.writeToStdOut(node.root());
            throw new InvalidSelector(term);

        } catch (ClassCastException e) {
            throw new InvalidSelector(e, term);
        }
    }

    default LinkedNode node(String term) throws InvalidSelector {
        final LinkedContainer container = container(term);

        if (container == null || container.nodes().isEmpty()) {
            return null;
        }

        if (container.nodes().size() != 1) {
            throw new InvalidSelector(term);
        }
        return container.node();
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

    default <T extends Linkable> T literal(String term, Class<T> clazz) throws InvalidSelector {
        return literal(term, clazz, Function.identity());
    }

    default <T extends Linkable, R> R literal(String term, Class<T> clazz, Function<T, R> mapper) throws InvalidSelector {
        return literal(term, clazz, mapper, null);
    }

    default <T extends Linkable, R> R literal(String term, Class<T> clazz, Function<T, R> mapper, R defaultValue) throws InvalidSelector {

        Objects.requireNonNull(mapper);

        try {
            T value = materialize(term, clazz);

            if (value == null) {
                return defaultValue;
            }
            return mapper.apply(value);

        } catch (InvalidSelector e) {
            throw e;
        } catch (Exception e) {
            throw new InvalidSelector(e, term);
        }
    }

    default Instant xsdDateTime(String term) throws InvalidSelector {
        return literal(
                term,
                XsdDateTime.class,
                XsdDateTime::datetime);
    }

    default String lexicalValue(String term) throws InvalidSelector {
        return literal(
                term,
                LinkedLiteral.class,
                LinkedLiteral::lexicalValue);
    }

    default LinkedFragment fragment(String term) throws InvalidSelector {
        return literal(
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
            return null;
        } catch (IllegalArgumentException e) {

        }
        throw new InvalidSelector(JsonLdKeyword.ID);
    }

    default URI uri(String term) throws InvalidSelector {
        try {

            LinkedFragment node = materialize(term, LinkedFragment.class);

            if (node != null) {

                final String uri = node.asFragment().id().uri();
                if (uri != null) {
                    return URI.create(uri);
                }
            }
            return null;

        } catch (IllegalArgumentException | NodeAdapterError e) {
            throw new InvalidSelector(e, term);
        }
    }

    default LangStringSelector languageMap(String term) throws InvalidSelector {
        final LinkedContainer container = container(term);
        if (container != null) {
            return LanguageMap.of(container);
        }
        return LangStringSelector.empty();
    }

    default <T> Collection<T> collection(
            String term,
            Class<T> clazz) throws InvalidSelector {
        return collection(term, clazz, null);
    }

    @SuppressWarnings("unchecked")
    default <T> Collection<T> collection(
            String term,
            Class<T> clazz,
            NodeAdapter<LinkedNode, T> unmapped) throws InvalidSelector {

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
                                        
                } else if (node.isLiteral() && clazz.isInstance(node.asLiteral().cast())) {
                    collection.add(node.asLiteral().cast(clazz));

                } else if (unmapped != null) {
                    if (node.isFragment()
                            && node.asFragment().id() != null) {
                        collection.add(unmapped.materialize(node.asFragment().id().target()));
                    } else {
                        collection.add(unmapped.materialize(node));
                    }
                } else {
                    throw new InvalidSelector(term);
                }
            }

            return collection;

        } catch (NodeAdapterError e) {
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
