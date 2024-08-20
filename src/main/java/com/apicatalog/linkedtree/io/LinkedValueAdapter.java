package com.apicatalog.linkedtree.io;

import com.apicatalog.linkedtree.LinkedValue;

public interface LinkedValueAdapter<I, O extends LinkedValue> {
    
    String datatype();
    
    O read(I value, String language, String direction);
}
