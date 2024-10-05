package com.apicatalog.linkedtree.orm;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

@Retention(RetentionPolicy.RUNTIME)
@Target(value = { ElementType.TYPE, ElementType.METHOD })
public @interface Vocab {

    /**
     * A vocabulary URI used to expand a relative term into an absolute URI.
     * 
     * @return a vocabulary URI
     */
    String value();

}
