package com.apicatalog.linkedtree.literal;

import java.time.Instant;

import com.apicatalog.linkedtree.LinkedLiteral;

public interface DateTimeValue extends LinkedLiteral {

    Instant datetime();
    
}
