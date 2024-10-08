package com.apicatalog.linkedtree.orm;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.TYPE)
public @interface Fragment {
    
    /**
     * A generic fragment, i.e. untyped super type
     *   
     * @return <code>true</code> if the fragment is typed
     */
    boolean generic() default false;
}
