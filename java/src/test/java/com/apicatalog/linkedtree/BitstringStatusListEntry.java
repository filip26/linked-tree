package com.apicatalog.linkedtree;

import com.apicatalog.linkedtree.adapter.NodeAdapterError;

public record BitstringStatusListEntry() implements Status {
    
    public static String TYPE = "https://www.w3.org/ns/credentials/status#BitstringStatusListEntry";

    
    public static BitstringStatusListEntry of(LinkedFragment source) throws NodeAdapterError {
     
        var status = new BitstringStatusListEntry();
        
        
        return status;
    }
}
