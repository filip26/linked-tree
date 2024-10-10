package com.apicatalog.linkedtree.literal;

import java.time.Instant;
import java.util.Date;

import com.apicatalog.linkedtree.LinkedLiteral;

public interface DateTimeValue extends LinkedLiteral {

    Instant datetime();

    static Date toDate(DateTimeValue literal) {
        final Instant instant = literal.datetime();
        if (instant != null) {
            return Date.from(instant);
        }
        return null;
    }

}
