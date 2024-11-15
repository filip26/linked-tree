package com.apicatalog.linkedtree.orm.getter;

import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.logging.Logger;

public class GetterMethod {

    private static final Logger LOGGER = Logger.getLogger(GetterMethod.class.getName());
    
    public static Collection<Method> filter(Class<?> type, boolean includeDefault) {
        
        if (type.getMethods() == null || type.getMethods().length == 0) {
            return Collections.emptyList();
        }
        
        Collection<Method> getters = new ArrayList<Method>(type.getMethods().length);
        
        for (Method method : type.getMethods()) {
            
            if (!includeDefault && method.isDefault() 
                    || method.isSynthetic()
                    || Modifier.isStatic(method.getModifiers())
                    || Modifier.isNative(method.getModifiers())
                    || Modifier.isVolatile(method.getModifiers())
                    || Modifier.isTransient(method.getModifiers())
                    || void.class.equals(method.getReturnType())
                    || Void.class.equals(method.getReturnType())
                    ) {
                continue;
            }
            
            if (method.getParameterCount() > 0) {
//                LOGGER.log(Level.FINER, "Skipped method {0} - not a getter, has {1} paramters", new Object[] { method.toGenericString(), (Integer) method.getParameterCount() });
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
