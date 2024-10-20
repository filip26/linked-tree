package com.apicatalog.linkedtree.orm;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

import com.apicatalog.linkedtree.LinkedLiteral;
import com.apicatalog.linkedtree.orm.mapper.LiteralMapper;

@Retention(RetentionPolicy.RUNTIME)
@Target(value = { ElementType.METHOD, ElementType.ANNOTATION_TYPE })
@Deprecated
public @interface Mapper {

    Class<? extends LiteralMapper<? extends LinkedLiteral, ?>> value();

    String[] params() default {};
}
