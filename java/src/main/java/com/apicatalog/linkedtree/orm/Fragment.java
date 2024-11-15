package com.apicatalog.linkedtree.orm;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

import com.apicatalog.linkedtree.Linkable;
import com.apicatalog.linkedtree.orm.proxy.PropertyValueConsumer;

@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.TYPE)
public @interface Fragment {

    /**
     * A generic fragment, i.e. untyped super type
     * 
     * @return <code>true</code> if the interface denotes a type
     */
    boolean generic() default false;

    /**
     * Add {@link PropertyValueConsumer} interface to a materialized instance. The
     * interface is added automatically if {@link Provided} is present on a method.
     * 
     * @return <code>true</code> if mutable
     */
    boolean mutable() default false;

    /**
     * Add {@link Linkable} interface to a materialized instance.
     * 
     * @return
     */
    boolean linkable() default true;
}
