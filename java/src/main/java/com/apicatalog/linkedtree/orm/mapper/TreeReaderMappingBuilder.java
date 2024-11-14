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
import com.apicatalog.linkedtree.json.JsonLiteral;
import com.apicatalog.linkedtree.lang.LanguageMap;
import com.apicatalog.linkedtree.literal.ByteArrayValue;
import com.apicatalog.linkedtree.literal.DateTimeValue;
import com.apicatalog.linkedtree.literal.DoubleValue;
import com.apicatalog.linkedtree.literal.IntegerValue;
import com.apicatalog.linkedtree.literal.adapter.DataTypeAdapter;
import com.apicatalog.linkedtree.orm.Adapter;
import com.apicatalog.linkedtree.orm.Compaction;
import com.apicatalog.linkedtree.orm.Fragment;
import com.apicatalog.linkedtree.orm.Id;
import com.apicatalog.linkedtree.orm.Injected;
import com.apicatalog.linkedtree.orm.Literal;
import com.apicatalog.linkedtree.orm.Provided;
import com.apicatalog.linkedtree.orm.Term;
import com.apicatalog.linkedtree.orm.Type;
import com.apicatalog.linkedtree.orm.Vocab;
import com.apicatalog.linkedtree.orm.getter.CollectionGetter;
import com.apicatalog.linkedtree.orm.getter.FragmentGetter;
import com.apicatalog.linkedtree.orm.getter.Getter;
import com.apicatalog.linkedtree.orm.getter.GetterMethod;
import com.apicatalog.linkedtree.orm.getter.IdGetter;
import com.apicatalog.linkedtree.orm.getter.InjectedGetter;
import com.apicatalog.linkedtree.orm.getter.LangMapGetter;
import com.apicatalog.linkedtree.orm.getter.LiteralGetter;
import com.apicatalog.linkedtree.orm.getter.NodeGetter;
import com.apicatalog.linkedtree.orm.getter.RefGetter;
import com.apicatalog.linkedtree.orm.getter.TypeGetter;
import com.apicatalog.linkedtree.orm.proxy.FragmentProxy;
import com.apicatalog.linkedtree.type.FragmentType;
import com.apicatalog.linkedtree.type.GenericTypeAdapter;
import com.apicatalog.linkedtree.type.TypeAdapter;

import jakarta.json.JsonValue;

public class TreeReaderMappingBuilder {

    private static final Logger LOGGER = Logger.getLogger(TreeReaderMappingBuilder.class.getName());

    final Map<Class<?>, NodeAdapter<LinkedFragment, ?>> injectors;
    final Map<Class<?>, TypeAdapter> typeAdapters;
    final Map<Class<? extends DataTypeAdapter>, DataTypeAdapter> literalAdapters;

    final LiteralMapping literalMapping;

    protected TreeReaderMappingBuilder() {
        this.injectors = new LinkedHashMap<>();
        this.typeAdapters = new LinkedHashMap<>();
        this.literalAdapters = new LinkedHashMap<>();

        this.literalMapping = new LiteralMapping();
    }

    public TreeReaderMappingBuilder defaults() {
        literalMapping
                .add(LinkedLiteral.class, String.class,
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
                        DateTimeValue::toDate)
                .add(JsonLiteral.class, JsonValue.class,
                        JsonLiteral::jsonValue);
        return this;
    }

    public TreeReaderMappingBuilder with(TypeAdapter adapter) {
        this.typeAdapters.put(adapter.typeInterface(), adapter);
        return this;
    }

    public TreeReaderMappingBuilder with(
            String type,
            Class<?> typeInterface,
            NodeAdapter<LinkedFragment, Object> adapter) {
        return with(new GenericTypeAdapter(type, typeInterface, adapter));
    }

    public TreeReaderMappingBuilder with(DataTypeAdapter adapter) {
        this.literalAdapters.put(adapter.getClass(), adapter);
        return this;
    }

    public <T extends LinkedLiteral, R> TreeReaderMappingBuilder map(Class<T> source, Class<R> target, LiteralMapper<T, R> mapper) {
        literalMapping.add(source, target, mapper);
        return this;
    }

    public TreeReaderMappingBuilder scan(final Class<?> typeInterface) {
        return scan(typeInterface, false);
    }

    public TreeReaderMappingBuilder scan(final Class<?> typeInterface, boolean eager) {

        if (typeAdapters.containsKey(typeInterface)) {
            return this;
        }

        FragmentProxy proxy = proxy(typeInterface);

        if (proxy == null) {
            return this;
        }

        typeAdapters.put(
                typeInterface,
                proxy);

        return this;
    }

    public FragmentProxy proxy(final Class<?> typeInterface) {

        final Fragment fragment = typeInterface.getAnnotation(Fragment.class);

        if (fragment == null) {
            LOGGER.log(Level.WARNING, "Skipped class {0} - not annotated as @Fragment", typeInterface);
            return null;
        }

        final Vocab vocab = typeInterface.getAnnotation(Vocab.class);

        String typeName = null;

        if (!fragment.generic()) {
            typeName = expand(vocab, typeInterface.getAnnotation(Term.class), typeInterface.getSimpleName());
        }

        boolean mutable = fragment.mutable();

        final Map<Method, Getter> getters = new HashMap<>(typeInterface.getMethods().length);

        // scan methods
        for (final Method method : GetterMethod.filter(typeInterface, false)) {

            if (method.isAnnotationPresent(Provided.class)) {
                mutable = true;
                continue;
            }

            if (method.isAnnotationPresent(Injected.class)) {
                getters.put(method, injector(method));
                continue;
            }

            final Vocab declaredVocab = method.getDeclaringClass().getAnnotation(Vocab.class);
            Vocab methodVocab = method.getAnnotation(Vocab.class);
            final Term methodTerm = method.getAnnotation(Term.class);

            final boolean isIdMethod = method.isAnnotationPresent(Id.class);
            final boolean isTypeMethod = method.isAnnotationPresent(Type.class)
                    || method.getReturnType().isAssignableFrom(FragmentType.class);
            final boolean isLangMap = method.getReturnType().isAssignableFrom(LanguageMap.class);

            // ignore if no annotation is found
            if (methodVocab == null
                    && methodTerm == null
                    && !isIdMethod
                    && !isTypeMethod
                    && !isLangMap
                    && !method.isAnnotationPresent(Literal.class)
                    && !method.isAnnotationPresent(Adapter.class)
                    && !method.isAnnotationPresent(Compaction.class)) {
                continue;
            }
//            Mapper mapper = method.getAnnotation(Mapper.class);
//            if (mapper != null) {
//                try {
//                    Method mapMethod = Arrays.stream(mapper.value().getDeclaredMethods())
//                            .filter(m -> !m.isDefault() && !m.isSynthetic())
//                            .filter(m -> "map".equals(m.getName())
//                                    && m.getParameterCount() == 1
//                                    && LinkedLiteral.class.isAssignableFrom(m.getParameters()[0].getType()))
//                            .findFirst()
//                            .orElseThrow(() -> new IllegalStateException("LiteralMapper.map(LinkedLiteral) method is invalid."));
//
//                    Class<?> mapperReturnType = mapMethod.getReturnType();
//
//                    if (!method.getReturnType().isAssignableFrom(mapperReturnType)) {
//                        throw new IllegalArgumentException("Provider mapper return type [" + mapperReturnType + "] does not match method return type [" + method.getReturnType() + "].");
//                    }
//
//                    literalMapping.add(
//                            (Class) mapMethod.getParameterTypes()[0],
//                            method.getReturnType(),
//                            (LiteralMapper) mapper.value().getDeclaredConstructor().newInstance());
//
//                } catch (InvocationTargetException | IllegalAccessException
//                        | InstantiationException | NoSuchMethodException e) {
//                    throw new IllegalStateException(e);
//                }
//            }

            Getter getter = null;

            // @id
            if (method.isAnnotationPresent(Id.class)) {
                getter = IdGetter.instance();

                // @type
            } else if (method.isAnnotationPresent(Type.class)
                    || method.getReturnType().isAssignableFrom(FragmentType.class)) {
                getter = TypeGetter.instance(method);

            } else {

                if (methodVocab == null) {
                    methodVocab = declaredVocab;
                }

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
                        getter = CollectionGetter.of(
                                termUri,
                                method.getReturnType(),
                                componentClass,
                                source -> mapper(method).object(source.asLiteral()));
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
                    getter = new LiteralGetter(
                            termUri,
                            method.getReturnType(),
                            mapper(method));
                }
            }

            if (getter == null) {
                LOGGER.log(Level.WARNING, "An unknown method {0}", method);
            } else {
                getters.put(method, getter);
            }
        }

        return FragmentProxy.of(
                typeInterface,
                typeName,
                getters,
                mutable,
                true,
                fragment.linkable());
    }

    Getter injector(Method method) {
        Injected injected = method.getAnnotation(Injected.class);

        NodeAdapter<LinkedFragment, ?> adapter = injectors.get(injected.value());

        if (adapter == null) {
            try {
                adapter = injected.value().getConstructor().newInstance();
                injectors.put(injected.value(), adapter);
            } catch (InstantiationException | IllegalAccessException | IllegalArgumentException | InvocationTargetException | NoSuchMethodException | SecurityException e) {
                throw new IllegalStateException(e);
            }
        }
        return new InjectedGetter(adapter);
    }

    LiteralMapper<LinkedLiteral, ?> mapper(Method method) {

        Adapter adapterType = method.getAnnotation(Adapter.class);
        DataTypeAdapter adapter = null;

        if (adapterType != null) {

            adapter = literalAdapters.get(adapterType.value());

            if (adapter == null) {
                try {
                    Constructor<? extends DataTypeAdapter> constructor = adapterType.value().getDeclaredConstructor();
                    constructor.setAccessible(true);

                    adapter = constructor.newInstance();

                    literalAdapters.put(adapterType.value(), adapter);

                } catch (InstantiationException | IllegalAccessException | IllegalArgumentException | InvocationTargetException | NoSuchMethodException | SecurityException e) {
                    throw new IllegalStateException(e);
                }
            }
        }

        Literal literal = method.getAnnotation(Literal.class);

        if (literal != null) {

            Class<? extends LiteralMapper<? extends LinkedLiteral, ?>> mapperType = literal.value();

            if (mapperType != null) {
                try {
                    Constructor<? extends LiteralMapper<? extends LinkedLiteral, ?>> constructor = mapperType.getDeclaredConstructor();
                    constructor.setAccessible(true);

                    LiteralMapper<? extends LinkedLiteral, ?> mapping = constructor.newInstance();

                    return (LiteralMapper<LinkedLiteral, ?>) mapping;

                } catch (InstantiationException | IllegalAccessException | IllegalArgumentException | InvocationTargetException | NoSuchMethodException | SecurityException e) {
                    throw new IllegalStateException(e);
                }
            }
        }

        Class<?> type = method.getReturnType();

        if (adapter != null) {
            LiteralMapper<LinkedLiteral, ?> mapping = literalMapping.find(adapter.typeInterface(), type);
            if (mapping != null) {
                return mapping;
            }
        }

        final LiteralMapper<LinkedLiteral, ?> mapping;

        if (type.isAssignableFrom(LinkedLiteral.class)
                || type.isAssignableFrom(LinkedNode.class)) {
            mapping = LiteralMapper.identity();

        } else if (type.isAssignableFrom(String.class)) {
            mapping = source -> source.lexicalValue();

        } else if (type.isAssignableFrom(JsonValue.class)) {
            mapping = source -> ((JsonLiteral) source).jsonValue();

        } else {
            mapping = literalMapping.find(LinkedLiteral.class, method.getReturnType());
        }

        if (mapping == null) {
            throw new IllegalArgumentException("Cannot find literal mapper from " + LinkedLiteral.class.getTypeName() + " to " + method.getReturnType().getTypeName() + " on " + method);
        }

        return mapping;
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

    public TreeReaderMapping build() {
        return new TreeReaderMapping(
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

    public Collection<String> contexts(Class<?> clazz) {
        // TODO Auto-generated method stub
        return null;
    }
}
