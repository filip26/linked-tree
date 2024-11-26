package com.apicatalog.linkedtree.literal;

import java.time.Instant;
import java.util.Date;

public interface DateTimeValue {

    Instant datetime();

    static Date toDate(DateTimeValue literal) {
        final Instant instant = literal.datetime();
        if (instant != null) {
            return Date.from(instant);
        }
        return null;
    }
}
