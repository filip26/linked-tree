package com.apicatalog.linkedtree.orm.mapper;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.ParameterizedType;
import java.net.URI;
import java.util.Collection;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Objects;
import java.util.logging.Level;
import java.util.logging.Logger;

import com.apicatalog.linkedtree.LinkedContainer;
import com.apicatalog.linkedtree.adapter.NodeAdapterError;
import com.apicatalog.linkedtree.jsonld.io.JsonLdTreeReader;
import com.apicatalog.linkedtree.lang.LanguageMap;
import com.apicatalog.linkedtree.orm.Fragment;
import com.apicatalog.linkedtree.orm.Id;
import com.apicatalog.linkedtree.orm.Literal;
import com.apicatalog.linkedtree.orm.Term;
import com.apicatalog.linkedtree.orm.Vocab;
import com.apicatalog.linkedtree.orm.adapter.NativeFragmentAdapter;
import com.apicatalog.linkedtree.orm.adapter.NativeLiteralAdapter;
import com.apicatalog.linkedtree.orm.getter.CollectionGetter;
import com.apicatalog.linkedtree.orm.getter.FragmentGetter;
import com.apicatalog.linkedtree.orm.getter.Getter;
import com.apicatalog.linkedtree.orm.getter.IdGetter;
import com.apicatalog.linkedtree.orm.getter.LangMapGetter;
import com.apicatalog.linkedtree.orm.getter.LiteralGetter;
import com.apicatalog.linkedtree.orm.getter.NodeGetter;
import com.apicatalog.linkedtree.orm.getter.RefGetter;
import com.apicatalog.linkedtree.orm.getter.StringGetter;
import com.apicatalog.linkedtree.orm.getter.TypeGetter;
import com.apicatalog.linkedtree.type.Type;

public class TreeMapperBuilder {

    private static final Logger LOGGER = Logger.getLogger(TreeMapperBuilder.class.getName());

    final Map<Class<?>, NativeFragmentAdapter> fragmentAdapters;
    final Map<Class<? extends NativeLiteralAdapter>, NativeLiteralAdapter> literalAdapters;

    public TreeMapperBuilder() {
        this.fragmentAdapters = new LinkedHashMap<>();
        this.literalAdapters = new LinkedHashMap<>();
    }

    public TreeMapperBuilder scan(Class<?> clazz) throws NodeAdapterError {

        if (fragmentAdapters.containsKey(clazz)) {
            return this;
        }

        Fragment fragment = clazz.getAnnotation(Fragment.class);

        if (fragment == null) {
            LOGGER.log(Level.WARNING, "Skipped class {0} - not annotated as @Fragment", clazz);
            return this;
        }

        Vocab vocab = clazz.getAnnotation(Vocab.class);

        String typeName = null;

        if (!fragment.generic()) {
            typeName = expand(vocab, clazz.getAnnotation(Term.class), clazz.getSimpleName());
        }

        Map<Method, Getter> getters = new HashMap<>(clazz.getMethods().length);

        for (Method method : clazz.getMethods()) {

            if (method.getParameterCount() > 0) {
                LOGGER.log(Level.WARNING, "Skipped method {0} - not a getter, has {1} paramters", new Object[] { method.toGenericString(), (Integer) method.getParameterCount() });
                continue;
            }

            Vocab declaredVocab = method.getDeclaringClass().getAnnotation(Vocab.class);

            Getter getter = null;

            if (method.isAnnotationPresent(Id.class)) {
                getter = IdGetter.instance();

            } else if (method.getReturnType().isAssignableFrom(Type.class)) {
                getter = TypeGetter.instance();

            } else {

                Vocab methodVocab = method.getAnnotation(Vocab.class);

                if (methodVocab == null) {
                    methodVocab = declaredVocab;
                }

                Term methodTerm = method.getAnnotation(Term.class);

                String termUri = expand(
                        methodVocab,
                        methodTerm,
                        method.getName());

                if (Collection.class.isAssignableFrom(method.getReturnType())) {
                    Class<?> componentClass = (Class<?>) ((ParameterizedType) method.getGenericReturnType()).getActualTypeArguments()[0];
                    if (componentClass.isAnnotationPresent(Fragment.class)) {
                        scan(componentClass);
                        getter = new CollectionGetter(termUri, componentClass, fragmentAdapters.get(componentClass));

                    } else {
//                        getter = scanValue(method.getReturnType(), termUri);
                    }
//                        getter = scanLiteral(method, termUri);
                    // TODO arrays
                } else if (method.getReturnType().isArray()) {
//                    throw new UnsupportedOperationException();

                } else if (method.isAnnotationPresent(Literal.class)) {
                    getter = scanLiteral(method, termUri);

                } else {
                    getter = scanValue(method.getReturnType(), termUri);
                }
            }

            if (getter == null) {
                LOGGER.log(Level.WARNING, "An unknown method {0}", method);
            } else {
                getters.put(method, getter);
            }

        }

        fragmentAdapters.put(clazz, new NativeFragmentAdapter(clazz, typeName, getters));

        return this;
    }

    Getter scanValue(Class<?> type, String termUri) throws NodeAdapterError {
        if (type.isAssignableFrom(LanguageMap.class)) {
            return new LangMapGetter(termUri);
        }
        if (type.isAssignableFrom(URI.class)) {
            return new RefGetter(termUri);
        }
        if (type.isAnnotationPresent(Fragment.class)) {
            scan(type);
            return new FragmentGetter(termUri, fragmentAdapters.get(type));
        }
        if (type.isAssignableFrom(LinkedContainer.class)) {
            return new NodeGetter(termUri, LinkedContainer.class);
        }
        if (type.isAssignableFrom(String.class)) {
            return new StringGetter(termUri);
        }
        return null;
    }

    Getter scanLiteral(Method method, String termUri) throws NodeAdapterError {

        Literal literal = method.getAnnotation(Literal.class);

        Class<? extends NativeLiteralAdapter> adapterType = literal.value();

        NativeLiteralAdapter adapter = literalAdapters.get(adapterType);

        
        if (adapter == null) {
            try {
                Constructor<? extends NativeLiteralAdapter> constructor = adapterType.getDeclaredConstructor();
                constructor.setAccessible(true);

                adapter = constructor.newInstance();

                adapter.setup(literal.params());
                
                literalAdapters.put(adapterType, adapter);

            } catch (InstantiationException | IllegalAccessException | IllegalArgumentException | InvocationTargetException | NoSuchMethodException | SecurityException e) {
                throw new IllegalStateException(e);
            }
        }

        return new LiteralGetter(
                termUri,
                method.getReturnType(),
                adapter);

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

    // TODO reverse, JsonLdTreeReader.of(Scanner)
    public TreeMapper build() {

        final JsonLdTreeReader.Builder builder = JsonLdTreeReader.create();

        fragmentAdapters.values().stream()
                .filter(e -> Objects.nonNull(e.typeName()))
                .forEach(e -> builder.with(e.typeName(), e));
        literalAdapters.values().stream()
                .map(NativeLiteralAdapter::literalAdapter)
                .forEach(builder::with);

        JsonLdTreeReader reader = builder.build();

        return new TreeMapper(reader, fragmentAdapters);
    }

}
