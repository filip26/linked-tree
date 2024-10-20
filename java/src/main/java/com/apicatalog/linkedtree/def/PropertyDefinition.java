package com.apicatalog.linkedtree.def;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;

public class PropertyDefinition {

    protected String vocab;
    protected String name;
    protected Method method;
    
    public PropertyDefinition(String name, String vocab, Method method) {
        this.name = name;
        this.method = method;
        this.vocab = vocab;
    }
    
    public Object invoke(Object object) {

        try {
            return method.invoke(object);

        } catch (IllegalAccessException | IllegalArgumentException | InvocationTargetException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }

        return null;
    }

    public String name() {
        return name;
    }

    public String vocab() {
        return vocab;
    }
}
