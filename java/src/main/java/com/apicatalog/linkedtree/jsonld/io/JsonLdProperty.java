package com.apicatalog.linkedtree.jsonld.io;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;

public class JsonLdProperty {

    protected String name;
    protected Method method;
    
    public JsonLdProperty(String name, Method method) {
        this.name = name;
        this.method = method;
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

}
