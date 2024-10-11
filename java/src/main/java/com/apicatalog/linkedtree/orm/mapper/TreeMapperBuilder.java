package com.apicatalog.linkedtree.orm.mapper;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.ParameterizedType;
import java.net.URI;
import java.time.Instant;
import java.util.Collection;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.function.Function;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.stream.Collectors;

import com.apicatalog.linkedtree.LinkedContainer;
import com.apicatalog.linkedtree.LinkedFragment;
import com.apicatalog.linkedtree.LinkedLiteral;
import com.apicatalog.linkedtree.LinkedNode;
import com.apicatalog.linkedtree.adapter.NodeAdapter;
import com.apicatalog.linkedtree.adapter.NodeAdapterError;
import com.apicatalog.linkedtree.lang.LanguageMap;
import com.apicatalog.linkedtree.literal.ByteArrayValue;
import com.apicatalog.linkedtree.literal.DateTimeValue;
import com.apicatalog.linkedtree.literal.DoubleValue;
import com.apicatalog.linkedtree.literal.IntegerValue;
import com.apicatalog.linkedtree.literal.adapter.DataTypeAdapter;
import com.apicatalog.linkedtree.orm.Fragment;
import com.apicatalog.linkedtree.orm.Id;
import com.apicatalog.linkedtree.orm.Literal;
import com.apicatalog.linkedtree.orm.Term;
import com.apicatalog.linkedtree.orm.Vocab;
import com.apicatalog.linkedtree.orm.getter.CollectionGetter;
import com.apicatalog.linkedtree.orm.getter.FragmentGetter;
import com.apicatalog.linkedtree.orm.getter.Getter;
import com.apicatalog.linkedtree.orm.getter.IdGetter;
import com.apicatalog.linkedtree.orm.getter.LangMapGetter;
import com.apicatalog.linkedtree.orm.getter.LiteralGetter;
import com.apicatalog.linkedtree.orm.getter.NodeGetter;
import com.apicatalog.linkedtree.orm.getter.RefGetter;
import com.apicatalog.linkedtree.orm.getter.TypeGetter;
import com.apicatalog.linkedtree.orm.proxy.FragmentProxy;
import com.apicatalog.linkedtree.type.GenericTypeAdapter;
import com.apicatalog.linkedtree.type.Type;
import com.apicatalog.linkedtree.type.TypeAdapter;

public class TreeMapperBuilder {

    private static final Logger LOGGER = Logger.getLogger(TreeMapperBuilder.class.getName());

    final Map<Class<?>, TypeAdapter> typeAdapters;
    final Map<Class<? extends DataTypeAdapter>, DataTypeAdapter> literalAdapters;

    final LiteralMapping literalMapping;

    protected TreeMapperBuilder() {
        this.typeAdapters = new LinkedHashMap<>();
        this.literalAdapters = new LinkedHashMap<>();

        this.literalMapping = new LiteralMapping();
    }

    public TreeMapperBuilder defaults() {
        literalMapping.add(LinkedLiteral.class, String.class,
                LinkedLiteral::lexicalValue)
                .add(ByteArrayValue.class, byte[].class,
                        ByteArrayValue::byteArrayValue)
                .add(IntegerValue.class, int.class,
                        IntegerValue::integerValue)
                .add(DoubleValue.class, double.class,
                        DoubleValue::doubleValue)
                .add(DateTimeValue.class, Instant.class,
                        DateTimeValue::datetime)
                .add(DateTimeValue.class, Date.class,
                        DateTimeValue::toDate);
        return this;
    }

    public TreeMapperBuilder with(TypeAdapter adapter) {
        this.typeAdapters.put(adapter.typeInterface(), adapter);
        return this;
    }

    public TreeMapperBuilder with(
            String type,
            Class<?> typeInterface,
            NodeAdapter<LinkedFragment, Object> adapter) {
        return with(new GenericTypeAdapter(type, typeInterface, adapter));
    }

    public TreeMapperBuilder with(DataTypeAdapter adapter) {
        this.literalAdapters.put(adapter.getClass(), adapter);
        return this;
    }

    public <T extends LinkedLiteral, R> TreeMapperBuilder map(Class<T> source, Class<R> target, LiteralMapper<T, R> mapper) {
        literalMapping.add(source, target, mapper);
        return this;
    }

    public TreeMapperBuilder scan(final Class<?> clazz) throws NodeAdapterError {

        if (typeAdapters.containsKey(clazz)) {
            return this;
        }

        final Fragment fragment = clazz.getAnnotation(Fragment.class);

        if (fragment == null) {
            LOGGER.log(Level.WARNING, "Skipped class {0} - not annotated as @Fragment", clazz);
            return this;
        }

        final Vocab vocab = clazz.getAnnotation(Vocab.class);

        String typeName = null;

        if (!fragment.generic()) {
            typeName = expand(vocab, clazz.getAnnotation(Term.class), clazz.getSimpleName());
        }

        final Map<Method, Getter> getters = new HashMap<>(clazz.getMethods().length);

        // scan methods
        for (final Method method : clazz.getMethods()) {

            if (method.getParameterCount() > 0) {
                LOGGER.log(Level.WARNING, "Skipped method {0} - not a getter, has {1} paramters", new Object[] { method.toGenericString(), (Integer) method.getParameterCount() });
                continue;
            }

            final Vocab declaredVocab = method.getDeclaringClass().getAnnotation(Vocab.class);

            Getter getter = null;

            // @id
            if (method.isAnnotationPresent(Id.class)) {
                getter = IdGetter.instance();

                // @type
            } else if (method.getReturnType().isAssignableFrom(Type.class)) {
                getter = TypeGetter.instance();

            } else {

                Vocab methodVocab = method.getAnnotation(Vocab.class);

                if (methodVocab == null) {
                    methodVocab = declaredVocab;
                }

                final Term methodTerm = method.getAnnotation(Term.class);

                final String termUri = expand(
                        methodVocab,
                        methodTerm,
                        method.getName());

                if (Collection.class.isAssignableFrom(method.getReturnType())) {
                    Class<?> componentClass = (Class<?>) ((ParameterizedType) method.getGenericReturnType()).getActualTypeArguments()[0];
                    if (componentClass.isAnnotationPresent(Fragment.class)) {
                        scan(componentClass);
                        getter = CollectionGetter.of(
                                termUri,
                                method.getReturnType(),
                                componentClass,
                                typeAdapters.get(componentClass));

                    } else if (componentClass.isAssignableFrom(URI.class)) {
                        getter = CollectionGetter.of(
                                termUri,
                                method.getReturnType(),
                                componentClass,
                                source -> source.asFragment().uri());

                    } else {
                        LiteralMapper<LinkedLiteral, ?> mapper = mapper(method);
                        getter = CollectionGetter.of(
                                termUri,
                                method.getReturnType(),
                                componentClass,
                                source -> mapper.map(source.asLiteral()));
                    }

                    // TODO arrays
                } else if (method.getReturnType().isArray()) {
//                    throw new UnsupportedOperationException();

                } else if (method.getReturnType().isAnnotationPresent(Fragment.class)) {
                    scan(method.getReturnType());
                    getter = new FragmentGetter(termUri, typeAdapters.get(method.getReturnType()));

                } else if (method.getReturnType().isAssignableFrom(LanguageMap.class)) {
                    getter = new LangMapGetter(termUri);

                } else if (method.getReturnType().isAssignableFrom(URI.class)) {
                    getter = new RefGetter(termUri);

                } else if (method.getReturnType().isAssignableFrom(LinkedContainer.class)) {
                    getter = new NodeGetter(termUri, LinkedContainer.class);

                } else {

                    LiteralMapper<LinkedLiteral, ?> mapper = mapper(method);

                    getter = new LiteralGetter(
                            termUri,
                            method.getReturnType(),
                            mapper);
                }
            }

            if (getter == null) {
                LOGGER.log(Level.WARNING, "An unknown method {0}", method);
            } else {
                getters.put(method, getter);
            }

        }

        final FragmentProxy proxy = new FragmentProxy(clazz, typeName, getters);
        typeAdapters.put(clazz, proxy);

        return this;
    }

    LiteralMapper<LinkedLiteral, ?> mapper(Method method) throws NodeAdapterError {

        Literal literal = method.getAnnotation(Literal.class);

        if (literal != null) {

            Class<? extends DataTypeAdapter> adapterType = literal.value();

            DataTypeAdapter adapter = literalAdapters.get(adapterType);

            if (adapter == null) {
                try {
                    Constructor<? extends DataTypeAdapter> constructor = adapterType.getDeclaredConstructor();
                    constructor.setAccessible(true);

                    adapter = constructor.newInstance();

                    // TODO move to build, merge params
                    adapter.setup(literal.params());

                    literalAdapters.put(adapterType, adapter);

                } catch (InstantiationException | IllegalAccessException | IllegalArgumentException | InvocationTargetException | NoSuchMethodException | SecurityException e) {
                    throw new IllegalStateException(e);
                }
            }

            final LiteralMapper<LinkedLiteral, ?> mapper = literalMapping.find(adapter.typeInterface(), method.getReturnType());

            if (mapper == null) {
                throw new IllegalArgumentException("Cannot find literal mapper from " + adapter.typeInterface() + " to " + method.getReturnType());
            }

            return mapper;
        }

        Class<?> type = method.getReturnType();

        if (type.isAssignableFrom(LinkedLiteral.class)
                || type.isAssignableFrom(LinkedNode.class)) {
            return source -> source;
        }

        if (type.isAssignableFrom(String.class)) {
            return source -> source.lexicalValue();
        }

        // TODO generic adapters,
        throw new ClassCastException();
    }

    static final boolean isAbsoluteUri(final String uri, final boolean validate) {

        // if URI validation is disabled
        if (!validate) {
            // then validate just a scheme
            return startsWithScheme(uri);
        }

        if (uri == null
                || uri.length() < 3 // minimal form s(1):ssp(1)
        ) {
            return false;
        }

        try {
            return URI.create(uri).isAbsolute();
        } catch (IllegalArgumentException e) {
            return false;
        }
    }

    static final boolean startsWithScheme(final String uri) {

        if (uri == null
                || uri.length() < 2 // a scheme must have at least one letter followed by ':'
                || !Character.isLetter(uri.codePointAt(0)) // a scheme name must start with a letter
        ) {
            return false;
        }

        for (int i = 1; i < uri.length(); i++) {

            if (
            // a scheme name must start with a letter followed by a letter/digit/+/-/.
            Character.isLetterOrDigit(uri.codePointAt(i))
                    || uri.charAt(i) == '-' || uri.charAt(i) == '+' || uri.charAt(i) == '.') {
                continue;
            }

            // a scheme name must be terminated by ':'
            return uri.charAt(i) == ':';
        }
        return false;
    }

    static String expand(Vocab vocab, Term term, String nativeName) {

        String uri = nativeName;
        String prefix = vocab != null && !vocab.value().isBlank() ? vocab.value() : null;

        if (term != null) {
            if (!term.value().isBlank()) {
                uri = term.value();
            }
            if (!term.vocab().isBlank()) {
                prefix = term.vocab();
            }
        }

        return prefix == null || isAbsoluteUri(uri, false)
                ? uri
                : prefix + uri;
    }

    public TreeMapping build() {
        return new TreeMapping(
                Collections.unmodifiableMap(typeAdapters),
                typeAdapters.values().stream()
                        .filter(adapter -> adapter.type() != null)
                        .collect(Collectors.toUnmodifiableMap(
                                TypeAdapter::type,
                                Function.identity())),
                literalAdapters.values().stream()
                        .collect(Collectors.toUnmodifiableMap(
                                DataTypeAdapter::datatype,
                                Function.identity())));
    }
}