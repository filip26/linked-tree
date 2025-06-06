package com.apicatalog.linkedtree.orm.proxy;

import com.apicatalog.linkedtree.orm.Provided;

@FunctionalInterface
public interface PropertyValueConsumer {

    /**
     * The method name is intentionally long to avoid collisions.
     * 
     * @param getterName    a getter name that returns the value
     * @param propertyValue to set for a property annotated with {@link Provided}
     */
    void acceptFragmentPropertyValue(String getterName, Object propertyValue);
}
