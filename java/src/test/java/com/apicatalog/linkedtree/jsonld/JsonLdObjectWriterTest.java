package com.apicatalog.linkedtree.jsonld;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;
import java.time.Instant;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Stream;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.MethodSource;

import com.apicatalog.linkedtree.Linkable;
import com.apicatalog.linkedtree.TestUtils;
import com.apicatalog.linkedtree.adapter.NodeAdapterError;
import com.apicatalog.linkedtree.builder.TreeBuilderError;
import com.apicatalog.linkedtree.jsonld.io.JsonLdObjectWriter;
import com.apicatalog.linkedtree.jsonld.io.JsonLdTreeReader;
import com.apicatalog.linkedtree.orm.mapper.TreeMapping;
import com.apicatalog.linkedtree.orm.test.AlumniSubject;
import com.apicatalog.linkedtree.orm.test.AnnotatedCredential;
import com.apicatalog.linkedtree.orm.test.BitstringStatusListEntry;
import com.apicatalog.linkedtree.orm.test.ControllerDocument;
import com.apicatalog.linkedtree.orm.test.ExtendedAnnotatedCredential;
import com.apicatalog.linkedtree.orm.test.GenericMultikey;
import com.apicatalog.linkedtree.orm.test.GenericSubject;
import com.apicatalog.linkedtree.orm.test.JsonWebKey;
import com.apicatalog.linkedtree.orm.test.Multikey;
import com.apicatalog.linkedtree.orm.test.VerificationMethod;
import com.apicatalog.linkedtree.test.Status;

import jakarta.json.JsonArray;
import jakarta.json.JsonObject;

class JsonLdObjectWriterTest {

    static JsonLdTreeReader READER = JsonLdTreeReader.of(
            TreeMapping.createBuilder()
                    .scan(ControllerDocument.class)
                    .scan(JsonWebKey.class)
                    .scan(Multikey.class)
                    .scan(BitstringStatusListEntry.class)
                    .scan(ExtendedAnnotatedCredential.class)
                    .scan(AnnotatedCredential.class)
                    .build());

    static JsonLdObjectWriter WRITER = new JsonLdObjectWriter()
            .scan(ControllerDocument.class)
            .scan(Multikey.class)
            .scan(JsonWebKey.class)
            .scan(VerificationMethod.class)
            .scan(AnnotatedCredential.class)
            .scan(BitstringStatusListEntry.class)
            .scan(GenericSubject.class)
            .scan(Status.class)
            .scan(ExtendedAnnotatedCredential.class)
            .scan(AlumniSubject.class);

    static {
        WRITER.contextReducer()
                .define("https://www.w3.org/ns/controller/v1",
                        List.of("https://w3id.org/security/jwk/v1",
                                "https://w3id.org/security/multikey/v1"));
    }

    static Map<String, Class<?>> TYPES = new HashMap<>();

    static {
        TYPES.put("doc-1-in.jsonld", ControllerDocument.class);
        TYPES.put("jwk-1-in.jsonld", JsonWebKey.class);
        TYPES.put("multikey-4-in.jsonld", Multikey.class);
        TYPES.put("signed-vc-1-in.jsonld", ExtendedAnnotatedCredential.class);
    }

    @Test
    void testWriteMultikey() {

        JsonLdObjectWriter writer = new JsonLdObjectWriter()
                .scan(Multikey.class);

        Multikey multikey = new GenericMultikey(
                URI.create("urn:example:1234"),
                null,
                URI.create("https://example.org/controller/1"),
                Instant.parse("2024-01-01T00:00:00Z"),
                Instant.now(),
                () -> "publicKeyValue",
                () -> "privateKeyValue");

        JsonObject jsonld = writer.compacted(multikey);

        assertNotNull(jsonld);

        TestUtils.prettyPrint(jsonld);
    }

    @ParameterizedTest(name = "{0}")
    @MethodSource("resources")
    void compacted(String name, JsonArray input, JsonObject expected) throws TreeBuilderError, NodeAdapterError {

        Class<?> type = TYPES.get(name);

        var tree = READER.read(type, input);

        assertNotNull(tree);

        var output = WRITER.compacted(tree);

        assertNotNull(output);

        assertTrue(TestUtils.compareJson(name, ((Linkable) tree).ld(), output, expected));
    }

    static final Stream<Object[]> resources() throws IOException, URISyntaxException {
        return TestUtils.resources("jsonld/compacted", "-in.jsonld", ".jsonld");
    }
}
