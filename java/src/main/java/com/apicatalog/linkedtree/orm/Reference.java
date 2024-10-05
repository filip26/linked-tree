package com.apicatalog.linkedtree.orm;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.METHOD)
public @interface Reference {

    /**
     * A term name that could be a relative or an absolute URI. A method name is
     * used instead when undefined.
     * 
     * @return a term name, relative or absolute
     */
    String value() default "";

    /**
     * A vocabulary URI, ignored when {@link Reference#value()} is an absolute URI.
     * Takes precedence over {@link Vocab}.
     * 
     * @return a vocabulary URI
     */
    String vocab() default "";
}
