package com.apicatalog.linkedtree.orm;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

import com.apicatalog.linkedtree.LinkedFragment;
import com.apicatalog.linkedtree.adapter.NodeAdapter;

@Retention(RetentionPolicy.RUNTIME)
@Target(value = { ElementType.METHOD, ElementType.ANNOTATION_TYPE })
public @interface Injected {

    Class<? extends NodeAdapter<LinkedFragment, ?>> value();

}
