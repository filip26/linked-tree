package com.apicatalog.linkedtree.orm;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

import com.apicatalog.linkedtree.orm.adapter.NativeLiteralAdapter;

@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.METHOD)
public @interface Literal {

//    /**
//     * A term name that could be a relative or an absolute URI. A method name is
//     * used instead when undefined.
//     * 
//     * @return a term name, relative or absolute
//     */
//    String value() default "";
//
//    /**
//     * A vocabulary URI, ignored when {@link Literal#value()} is an absolute URI.
//     * Takes precedence over {@link Vocab}.
//     * 
//     * @return a vocabulary URI
//     */
//    String vocab() default "";
    
    Class<? extends NativeLiteralAdapter> value() default NativeLiteralAdapter.class;
    
    String[] params() default { };
}
