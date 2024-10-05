package com.apicatalog.ld;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.TYPE)
public @interface LdType {

    /**
     * A <code>@type</code> name that could be a relative or an absolute URI. It is
     * recommended to use a simple name and rely on vocabulary expansion.
     * 
     * @return a type name
     */
    String value();

    /**
     * A vocabulary URI, ignored when {@link LdType#value()} is an absolute URI.
     * Takes precedence over {@link LdVocab}.
     * 
     * @return a vocabulary URI
     */
    String vocab() default "";
}
