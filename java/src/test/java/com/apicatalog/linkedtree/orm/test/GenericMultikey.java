package com.apicatalog.linkedtree.orm.test;

import java.net.URI;
import java.time.Instant;

import com.apicatalog.linkedtree.type.Type;

public record GenericMultikey(
        URI id,
        Type type,
        URI controller,
        Instant revoked,
        Instant expires,
        EncodedKey publicKey,
        EncodedKey privateKey
        ) implements Multikey {
}
