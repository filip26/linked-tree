package com.apicatalog.linkedtree.jsonld;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;

import java.io.IOException;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.net.URISyntaxException;
import java.util.Collections;
import java.util.List;

import org.junit.jupiter.api.MethodOrderer.OrderAnnotation;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestMethodOrder;

import com.apicatalog.linkedtree.LinkedTree;
import com.apicatalog.linkedtree.builder.GenericTreeBuilder;
import com.apicatalog.linkedtree.jsonld.io.JsonLdTreeReader;
import com.apicatalog.linkedtree.jsonld.io.JsonLdTreeWriter;
import com.apicatalog.linkedtree.reader.LinkedReaderError;
import com.apicatalog.linkedtree.traversal.NodeSelector.ProcessingPolicy;
import com.apicatalog.linkedtree.xsd.XsdDateTime;

import jakarta.json.Json;
import jakarta.json.JsonArray;
import jakarta.json.JsonStructure;
import jakarta.json.JsonValue;
import jakarta.json.JsonWriter;
import jakarta.json.JsonWriterFactory;
import jakarta.json.stream.JsonGenerator;

@TestMethodOrder(OrderAnnotation.class)
class GenericTreeBuilderTest {

    static JsonLdTreeReader READER = JsonLdTreeReader.create()
            .with(VerifiableCredential.TYPE, VerifiableCredential::of)
            .with(Base64ByteArray.TYPE, Base64ByteArray::of)
            .with(XsdDateTime.TYPE, XsdDateTime::of)
            .build();

    static JsonLdTreeWriter WRITER = new JsonLdTreeWriter();

    @Test
    void genericClone() throws IOException, URISyntaxException, LinkedReaderError {

        JsonArray input = resource("custom/signed-vc-1.jsonld");

        LinkedTree tree = READER.read(
                List.of("https://www.w3.org/2018/credentials/v1",
                        "https://w3id.org/security/data-integrity/v2"),
                input);

        assertNotNull(tree);

        VerifiableCredential vc = tree
                .single(VerifiableCredential.class);

        assertNotNull(vc);

        GenericTreeBuilder builder = new GenericTreeBuilder(tree);
        var clone = builder.deepClone(
                (node, indexOrder, indexTerm, depth) -> "https://w3id.org/security#proof".equals(indexTerm)
                        ? ProcessingPolicy.Drop
                        : ProcessingPolicy.Accept);

        assertNotNull(clone);

        JsonArray out = WRITER.write(clone);

        JsonArray expected = resource("custom/unsigned-vc-1.jsonld");

        assertTrue(compareJson(null, out, expected));
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
