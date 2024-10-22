package com.apicatalog.linkedtree.jsonld;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;
import java.time.Instant;
import java.util.stream.Stream;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.MethodSource;

import com.apicatalog.linkedtree.TestUtils;
import com.apicatalog.linkedtree.adapter.NodeAdapterError;
import com.apicatalog.linkedtree.builder.TreeBuilderError;
import com.apicatalog.linkedtree.jsonld.io.JsonLdTreeReader;
import com.apicatalog.linkedtree.jsonld.io.ObjectJsonLdWriter;
import com.apicatalog.linkedtree.orm.mapper.TreeMapping;
import com.apicatalog.linkedtree.orm.test.GenericMultikey;
import com.apicatalog.linkedtree.orm.test.JsonWebKey;
import com.apicatalog.linkedtree.orm.test.Multikey;
import com.apicatalog.linkedtree.orm.test.VerificationMethod;

import jakarta.json.JsonArray;
import jakarta.json.JsonObject;

class ObjectJsonLdWriterTest {

    static JsonLdTreeReader READER = JsonLdTreeReader.of(
            TreeMapping.createBuilder()
                    .scan(JsonWebKey.class)
                    .scan(Multikey.class)
                    .build());


    static ObjectJsonLdWriter WRITER = new ObjectJsonLdWriter()
            .scan(Multikey.class)
            .scan(JsonWebKey.class)
            ;

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
    

    @DisplayName("Read/Write")
    @ParameterizedTest(name = "{0}")
    @MethodSource("resources")
    void readWrite(String name, JsonArray input, JsonObject expected) throws TreeBuilderError, NodeAdapterError {

        var tree = READER.read(input);

        assertNotNull(tree);

        var output = WRITER.writeCompact(tree.materialize(VerificationMethod.class));

        assertNotNull(output);

        assertTrue(TestUtils.compareJson(name, tree, expected, output));
    }
    
    static final Stream<Object[]> resources() throws IOException, URISyntaxException {
        return TestUtils.resources("jsonld/compacted", "-in.jsonld", ".jsonld");
    }


}
