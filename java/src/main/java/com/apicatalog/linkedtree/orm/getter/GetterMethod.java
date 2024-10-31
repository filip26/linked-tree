package com.apicatalog.linkedtree.orm.getter;

import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.logging.Level;
import java.util.logging.Logger;

public class GetterMethod {

    private static final Logger LOGGER = Logger.getLogger(GetterMethod.class.getName());
    
    public static Collection<Method> filter(Class<?> type) {
        
        if (type.getMethods() == null || type.getMethods().length == 0) {
            return Collections.emptyList();
        }
        
        Collection<Method> getters = new ArrayList<Method>(type.getMethods().length);
        
        for (Method method : type.getMethods()) {
            if (method.isDefault() || method.isSynthetic() || void.class.equals(method.getReturnType())) {
                continue;
            }

            if (method.getParameterCount() > 0) {
                LOGGER.log(Level.WARNING, "Skipped method {0} - not a getter, has {1} paramters", new Object[] { method.toGenericString(), (Integer) method.getParameterCount() });
                continue;
            }

            getters.add(method);
        }
        
        if (getters.isEmpty()) {
            return Collections.emptyList();
        }
        
        return getters;
    }
    
}
