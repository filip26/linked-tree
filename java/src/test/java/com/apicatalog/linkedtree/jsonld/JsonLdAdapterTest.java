package com.apicatalog.linkedtree.jsonld;

import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;

import java.io.IOException;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.net.URISyntaxException;
import java.time.Instant;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.MethodOrderer.OrderAnnotation;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestMethodOrder;

import com.apicatalog.linkedtree.LinkedTree;
import com.apicatalog.linkedtree.jsonld.io.JsonLdTreeReader;
import com.apicatalog.linkedtree.jsonld.io.JsonLdTreeWriter;
import com.apicatalog.linkedtree.literal.ByteArrayValue;
import com.apicatalog.linkedtree.reader.LinkedReaderError;
import com.apicatalog.linkedtree.writer.NodeDebugWriter;
import com.apicatalog.linkedtree.xsd.XsdDateTime;

import jakarta.json.Json;
import jakarta.json.JsonArray;
import jakarta.json.JsonStructure;
import jakarta.json.JsonValue;
import jakarta.json.JsonWriter;
import jakarta.json.JsonWriterFactory;
import jakarta.json.stream.JsonGenerator;

@DisplayName("JsonLd Adapters Suite")
@TestMethodOrder(OrderAnnotation.class)
class JsonLdAdapterTest {

    static JsonLdTreeReader READER = JsonLdTreeReader.create()
            .with(VerifiableCredential.TYPE, VerifiableCredential::of)
            .with(Base64ByteArray.TYPE, Base64ByteArray::of)
            .with(XsdDateTime.TYPE, XsdDateTime::of)
            .build();

    static JsonLdTreeWriter WRITER = new JsonLdTreeWriter();

    @Test
    void base64ByteArray() throws IOException, URISyntaxException, LinkedReaderError {

        JsonArray input = resource("custom/base64-1.jsonld");
        JsonArray output = resource("custom/base64-2.jsonld");

        var tree = READER.readExpanded(input);

        assertNotNull(tree);

        ByteArrayValue literal = tree
                .single()
                .asFragment()
                .property("http://example.org/test#property4")
                .single()
//TODO                .singleLiteral(ByteArrayValue.class);
                .asLiteral() // TODO as param
                .cast(ByteArrayValue.class);

        assertNotNull(literal);
        assertEquals("RW5jb2RlIHRvIEJhc2U2NCBmb3JtYXQ=", literal.lexicalValue());
        assertTrue(literal instanceof ByteArrayValue);
        assertArrayEquals("Encode to Base64 format".getBytes(), ((ByteArrayValue) literal).byteArrayValue());

        ((Base64ByteArray) literal).byteArrayValue("test X".getBytes());
        assertEquals("dGVzdCBY", literal.lexicalValue());

        JsonArray copy = WRITER.writeExpanded(tree);

        assertNotNull(output);

        assertTrue(compareJson("", copy, output));
    }

    @Test
    void verifiableCredential() throws IOException, URISyntaxException, LinkedReaderError {

        JsonArray input = resource("custom/signed-vc-1.jsonld");

        LinkedTree tree = READER.readExpanded(
                List.of("https://www.w3.org/2018/credentials/v1",
                        "https://w3id.org/security/data-integrity/v2"),
                input);

        assertNotNull(tree);

        VerifiableCredential vc = tree
                .single(VerifiableCredential.class);

        assertNotNull(vc);

        assertEquals("urn:uuid:58172aac-d8ba-11ed-83dd-0b3aef56cc33", vc.id.uri());

        assertEquals(new HashSet<>(Arrays.asList(new String[] {
                "https://www.w3.org/2018/credentials#VerifiableCredential",
                "https://www.w3.org/ns/credentials/examples#AlumniCredential"
        })), vc.type());

        assertEquals(1, vc.name.size());
        assertEquals("Alumni Credential", vc.name.single().lexicalValue());
        assertNull(vc.name.single().language());

        assertEquals(1, vc.name.strings().size());
        assertEquals(1, vc.name.langCodes().size());

        assertNotNull(vc.description);

        assertEquals(Instant.parse("2023-01-01T00:00:00Z"), vc.validFrom);

        assertNull(vc.validUntil);

        assertEquals(1, vc.subject().size());

        assertEquals("https://vc.example/issuers/5678", vc.issuer().id().uri());

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
