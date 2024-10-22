package com.apicatalog.linkedtree.jsonld;

import static org.junit.jupiter.api.Assertions.assertNotNull;

import java.net.URI;
import java.time.Instant;

import org.junit.jupiter.api.Test;

import com.apicatalog.linkedtree.TestUtils;
import com.apicatalog.linkedtree.jsonld.io.ObjectJsonLdWriter;
import com.apicatalog.linkedtree.orm.test.GenericMultikey;
import com.apicatalog.linkedtree.orm.test.Multikey;

import jakarta.json.JsonObject;

class ObjectJsonLdWriterTest {

    @Test
    void testWriteMultikey() {

        ObjectJsonLdWriter writer = new ObjectJsonLdWriter()
                .scan(Multikey.class);

        Multikey multikey = new GenericMultikey(
                URI.create("urn:example:1234"),
                null,
                URI.create("https://example.org/controller/1"),
                Instant.parse("2024-01-01T00:00:00Z"),
                Instant.now(),
                () -> "publicKeyValue",
                () -> "privateKeyValue");

        JsonObject jsonld = writer.writeCompact(multikey);

        assertNotNull(jsonld);

        TestUtils.prettyPrint(jsonld);
    }

}
