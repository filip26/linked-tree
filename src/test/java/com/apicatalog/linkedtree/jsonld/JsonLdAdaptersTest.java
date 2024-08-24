package com.apicatalog.linkedtree.jsonld;

import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;

import java.io.IOException;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.net.URISyntaxException;
import java.util.Collections;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.MethodOrderer.OrderAnnotation;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestMethodOrder;

import com.apicatalog.linkedtree.jsonld.io.JsonLdTreeReader;
import com.apicatalog.linkedtree.jsonld.io.JsonLdTreeWriter;
import com.apicatalog.linkedtree.literal.ByteArrayValue;
import com.apicatalog.linkedtree.xsd.adapter.XsdDateTimeAdapter;

import jakarta.json.Json;
import jakarta.json.JsonArray;
import jakarta.json.JsonStructure;
import jakarta.json.JsonValue;
import jakarta.json.JsonWriter;
import jakarta.json.JsonWriterFactory;
import jakarta.json.stream.JsonGenerator;

@DisplayName("JsonLd Adapters Suite")
@TestMethodOrder(OrderAnnotation.class)
class JsonLdAdaptersTest {

    static JsonLdTreeReader READER = JsonLdTreeReader
            .with(
                    new VerifiableCredentialAdapter(),
                    new Base64ByteArrayAdapter(),
                    new XsdDateTimeAdapter()
                    );

    static JsonLdTreeWriter WRITER = new JsonLdTreeWriter();

    @Test
    void base64ByteArray() throws IOException, URISyntaxException {

        JsonArray input = resource("custom/base64-1.jsonld");
        JsonArray output = resource("custom/base64-2.jsonld");

        var tree = READER.readExpanded(input);

        assertNotNull(tree);

        ByteArrayValue literal = tree
                .singleNode()
                .asFragment()
                .property("http://example.org/test#property4")
                .single()
//TODO                .singleLiteral(ByteArrayValue.class);
                .asLiteral() // TODO as param
                .cast(ByteArrayValue.class);

        assertNotNull(literal);
        assertEquals("RW5jb2RlIHRvIEJhc2U2NCBmb3JtYXQ=", literal.value());
        assertTrue(literal instanceof ByteArrayValue);
        assertArrayEquals("Encode to Base64 format".getBytes(), ((ByteArrayValue) literal).byteArrayValue());

        ((Base64ByteArray) literal).byteArrayValue("test X".getBytes());
        assertEquals("dGVzdCBY", literal.value());

        JsonArray copy = WRITER.writeExpanded(tree);

        assertNotNull(output);

        assertTrue(compareJson("", copy, output));
    }

    @Test
    void verifiableCredential() throws IOException, URISyntaxException {

        JsonArray input = resource("custom/signed-vc-1.jsonld");

        var tree = READER.readExpanded(input);

        assertNotNull(tree);

        VerifiableCredential vc = tree
                .singleNode()
                .asFragment()
                .id()
                .target()
                .cast(VerifiableCredential.class)
                ;
        
        
        assertNotNull(vc);
//        assertEquals("RW5jb2RlIHRvIEJhc2U2NCBmb3JtYXQ=", literal.value());
//        assertTrue(literal instanceof ByteArrayValue);
//        assertArrayEquals("Encode to Base64 format".getBytes(), ((ByteArrayValue) literal).byteArrayValue());
//
//        ((Base64ByteArray) literal).byteArrayValue("test X".getBytes());
//        assertEquals("dGVzdCBY", literal.value());

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
