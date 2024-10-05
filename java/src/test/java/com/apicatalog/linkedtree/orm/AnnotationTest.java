package com.apicatalog.linkedtree.orm;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;

import java.io.IOException;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.net.URI;
import java.net.URISyntaxException;
import java.time.Instant;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.stream.Collectors;

import org.junit.jupiter.api.Test;

import com.apicatalog.linkedtree.Linkable;
import com.apicatalog.linkedtree.LinkedFragment;
import com.apicatalog.linkedtree.LinkedTree;
import com.apicatalog.linkedtree.adapter.NodeAdapterError;
import com.apicatalog.linkedtree.builder.TreeBuilderError;
import com.apicatalog.linkedtree.jsonld.JsonLdComparison;
import com.apicatalog.linkedtree.jsonld.JsonLdKeyword;
import com.apicatalog.linkedtree.jsonld.io.JsonLdTreeReader;

import jakarta.json.Json;
import jakarta.json.JsonArray;
import jakarta.json.JsonStructure;
import jakarta.json.JsonValue;
import jakarta.json.JsonWriter;
import jakarta.json.JsonWriterFactory;
import jakarta.json.stream.JsonGenerator;

class AnnotationTest {

    @Test
    void x() throws IOException, URISyntaxException, TreeBuilderError, NodeAdapterError {

        JsonLdTreeReader reader = new TreeMapping()
                .scan(AnnotatedCredential.class)
//                .scan(ExtendedAnnotatedCredential.class)
                .newReader();

        JsonArray input = resource("custom/signed-vc-1.jsonld");

        LinkedTree tree = reader.read(
                List.of("https://www.w3.org/2018/credentials/v1",
                        "https://w3id.org/security/data-integrity/v2"),
                input);

        assertNotNull(tree);

        AnnotatedCredential vc = tree.materialize(AnnotatedCredential.class);
        assertVc(vc);

//        ExtendedAnnotatedCredential eac = vc.type().materialize(ExtendedAnnotatedCredential.class);
//        assertVc(eac);

    }

    static void assertVc(AnnotatedCredential vc) {
        assertNotNull(vc);
        assertTrue(vc instanceof Linkable);

        assertEquals(
                URI.create("urn:uuid:58172aac-d8ba-11ed-83dd-0b3aef56cc33"),
                vc.id());

        assertEquals(new HashSet<>(Arrays.asList(new String[] {
                "https://www.w3.org/2018/credentials#VerifiableCredential",
                "https://www.w3.org/ns/credentials/examples#AlumniCredential"
        })), vc.type().stream().collect(Collectors.toSet()));

        assertEquals(1, vc.name().size());
        assertEquals("Alumni Credential", vc.name().first().lexicalValue());
        assertNull(vc.name().locale("en-US"));
        assertNull(vc.name().first().language());

        assertEquals(1, vc.name().values().size());
        assertEquals(1, vc.name().languages().size());

        assertNotNull(vc.description());

        assertEquals(Instant.parse("2023-01-01T00:00:00Z"), vc.validFrom());

        assertNull(vc.validUntil());

        assertEquals(
                URI.create("https://vc.example/issuers/5678"),
                vc.issuer());

        assertTrue(vc instanceof Linkable);
        assertNotNull(((Linkable)vc).ld());
        assertTrue(((Linkable)vc).ld() instanceof LinkedFragment);
        
        assertNotNull(vc.subject());
    }

    static final JsonArray resource(String name) throws IOException, URISyntaxException {
        try (var reader = Json.createReader(JsonLdKeyword.class.getResourceAsStream(name))) {
            return reader.readArray();
        }
    }

    static final boolean compareJson(final String testCase, final JsonStructure result, final JsonStructure expected) {

        if (JsonLdComparison.equals(expected, result)) {
            return true;
        }

        write(testCase, result, expected, null);

        fail("Expected " + expected + ", but was" + result);
        return false;
    }

    static void write(final String testCase, final JsonStructure result, final JsonStructure expected, Exception error) {
        final StringWriter stringWriter = new StringWriter();

        try (final PrintWriter writer = new PrintWriter(stringWriter)) {
            writer.println("Test " + testCase);

            final JsonWriterFactory writerFactory = Json.createWriterFactory(Collections.singletonMap(JsonGenerator.PRETTY_PRINTING, true));

            if (expected != null) {
                write(writer, writerFactory, "Expected", expected);
                writer.println();

//            } else if (testCase.expectErrorCode != null) {
//                writer.println("Expected: " + testCase.expectErrorCode);
            }

            if (result != null) {
                write(writer, writerFactory, "Actual", result);
                writer.println();
            }
            if (error != null) {
                writer.println("Actual: ");
                error.printStackTrace(writer);
            }
        }

        System.out.println(stringWriter.toString());
    }

    static final void write(final PrintWriter writer, final JsonWriterFactory writerFactory, final String name, final JsonValue result) {

        writer.println(name + ":");

        final StringWriter out = new StringWriter();

        try (final JsonWriter jsonWriter = writerFactory.createWriter(out)) {
            jsonWriter.write(result);
        }

        writer.write(out.toString());
        writer.println();
    }

}
