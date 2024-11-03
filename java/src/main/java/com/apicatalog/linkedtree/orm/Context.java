package com.apicatalog.linkedtree.orm;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.TYPE)
public @interface Context {

    /**
     * An array of <code>@context</code> URIs
     * 
     * @return
     */
    String[] value();

    /**
     * Override all previous {@link Context} declarations
     * 
     * @return
     */
    boolean override() default false;
}
