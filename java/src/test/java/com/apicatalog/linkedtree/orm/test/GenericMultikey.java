package com.apicatalog.linkedtree.orm.test;

import java.net.URI;
import java.time.Instant;

import com.apicatalog.linkedtree.type.FragmentType;

public record GenericMultikey(
        URI id,
        FragmentType type,
        URI controller,
        Instant revoked,
        Instant expires,
        EncodedKey publicKey,
        EncodedKey privateKey
        ) implements Multikey {
}
