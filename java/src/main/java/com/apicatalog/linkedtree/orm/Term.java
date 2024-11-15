package com.apicatalog.linkedtree.orm;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

@Retention(RetentionPolicy.RUNTIME)
@Target({ ElementType.METHOD, ElementType.TYPE })
public @interface Term {

    /**
     * A <code>@type</code> name that could be a relative or an absolute URI. It is
     * recommended to use a simple name and rely on vocabulary expansion.
     * 
     * @return a type name
     */
    String value() default "";

    /**
     * A vocabulary URI, ignored when {@link Term#value()} is an absolute URI.
     * Takes precedence over {@link Vocab}.
     * 
     * @return a vocabulary URI
     */
    String vocab() default "";
    
    boolean compact() default true;
}
