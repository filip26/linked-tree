package com.apicatalog.linkedtree.orm;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.ParameterizedType;
import java.net.URI;
import java.util.Collection;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

import com.apicatalog.linkedtree.LinkedContainer;
import com.apicatalog.linkedtree.jsonld.io.JsonLdTreeReader;
import com.apicatalog.linkedtree.lang.LanguageMap;
import com.apicatalog.linkedtree.orm.adapter.NativeFragmentAdapter;
import com.apicatalog.linkedtree.orm.adapter.NativeLiteralAdapter;
import com.apicatalog.linkedtree.orm.getter.FragmentGetter;
import com.apicatalog.linkedtree.orm.getter.Getter;
import com.apicatalog.linkedtree.orm.getter.IdGetter;
import com.apicatalog.linkedtree.orm.getter.LangMapGetter;
import com.apicatalog.linkedtree.orm.getter.LiteralGetter;
import com.apicatalog.linkedtree.orm.getter.NodeGetter;
import com.apicatalog.linkedtree.orm.getter.RefGetter;
import com.apicatalog.linkedtree.orm.getter.StringGetter;
import com.apicatalog.linkedtree.orm.getter.TermGetter;
import com.apicatalog.linkedtree.orm.getter.TypeGetter;
import com.apicatalog.linkedtree.type.Type;

public class TreeMapping {

    private static final Logger LOGGER = Logger.getLogger(TreeMapping.class.getName());

    final Map<Class<?>, NativeFragmentAdapter> fragmentAdapters;
    final Map<Class<? extends NativeLiteralAdapter>, NativeLiteralAdapter> literalAdapters;

    public TreeMapping() {
        this.fragmentAdapters = new LinkedHashMap<>();
        this.literalAdapters = new LinkedHashMap<>();
    }

    public TreeMapping scan(Class<?> clazz) {

        if (fragmentAdapters.containsKey(clazz)) {
            return this;
        }
        
        Vocab vocab = clazz.getAnnotation(Vocab.class);
        Term term = clazz.getAnnotation(Term.class);

        String typeName = expand(vocab, term, clazz.getSimpleName());

        Map<Method, Getter> getters = new HashMap<>(clazz.getMethods().length);

        for (Method method : clazz.getMethods()) {

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

                if (method.getReturnType().isAssignableFrom(LanguageMap.class)) {
                    getter = new LangMapGetter(termUri);

                } else if (method.isAnnotationPresent(Reference.class)
                        || method.getReturnType().isAssignableFrom(URI.class)) {
                    getter = new RefGetter(termUri);

                } else if (method.isAnnotationPresent(Literal.class)) {
                    getter = scanLiteral(method, termUri);

                } else if (method.getReturnType().isAnnotationPresent(Fragment.class)) {
                    scan(method.getReturnType());
                    getter = new FragmentGetter(termUri, fragmentAdapters.get(method.getReturnType()));

                } else if (method.getReturnType().isAssignableFrom(LinkedContainer.class)) {
                    getter = new NodeGetter(termUri, LinkedContainer.class);
                    
                } else if (method.getReturnType().isAssignableFrom(String.class)) {
                    getter = new StringGetter(termUri);
                }

//                System.out.println(method);

                if (Collection.class.isAssignableFrom(method.getReturnType())) {
                    Class<?> componentClass = (Class<?>) ((ParameterizedType) method.getGenericReturnType()).getActualTypeArguments()[0];
//                    System.out.println(componentClass);
                }

                if (getter == null) {
                    getter = new TermGetter(termUri);
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

    Getter scanLiteral(Method method, String termUri) {

        Literal literal = method.getAnnotation(Literal.class);

        Class<? extends NativeLiteralAdapter> adapterType = literal.adapter();

        NativeLiteralAdapter adapter = literalAdapters.get(adapterType);

        if (adapter == null) {

            try {
                adapter = adapterType.getDeclaredConstructor().newInstance();
                literalAdapters.put(adapterType, adapter);

            } catch (InstantiationException | IllegalAccessException | IllegalArgumentException | InvocationTargetException | NoSuchMethodException | SecurityException e) {
                // TODO Auto-generated catch block
                e.printStackTrace();
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
    public JsonLdTreeReader newReader() {

        final JsonLdTreeReader.Builder builder = JsonLdTreeReader.create();

        fragmentAdapters.values().forEach(e -> builder.with(e.typeName(), e));
        literalAdapters.values().forEach(builder::with);

        return builder.build();
    }

}
