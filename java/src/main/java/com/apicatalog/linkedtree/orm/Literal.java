package com.apicatalog.linkedtree.orm;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

import com.apicatalog.linkedtree.orm.adapter.LiteralMapper;

@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.METHOD)
public @interface Literal {

    Class<? extends LiteralMapper> value();

    String[] params() default {};
}
