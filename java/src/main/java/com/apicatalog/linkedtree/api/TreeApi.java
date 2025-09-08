package com.apicatalog.linkedtree.api;

import java.util.function.Function;
import java.util.function.Predicate;

public interface TreeApi {

    static final String[] ROOT = new String[] {"/"};
    
    enum TreeNodeValueType {
        String,
        Number,
        Binary,
        Boolean
    }
    
//    <T> T get(String[] path, Predicate<TreeNodeValueType> p);
    <T> T get(String[] path, TreeNodeValueType...t);
    /*
     * 
     */
    public static void main(String[] args) {
       
        TreeApi api;
        
//        api.get(ROOT, TreeNodeValueType.String);
        
    }
}
