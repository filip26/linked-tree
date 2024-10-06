package com.apicatalog.linkedtree.orm;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

import com.apicatalog.linkedtree.orm.adapter.NativeLiteralAdapter;

@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.METHOD)
public @interface Literal {

    Class<? extends NativeLiteralAdapter> value();

    String[] params() default {};
}
