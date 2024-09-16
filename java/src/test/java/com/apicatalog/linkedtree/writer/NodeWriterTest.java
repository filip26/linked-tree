package com.apicatalog.linkedtree.writer;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.fail;

import java.io.IOException;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.net.URISyntaxException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.Collections;
import java.util.stream.Stream;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.MethodOrderer.OrderAnnotation;
import org.junit.jupiter.api.TestMethodOrder;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.MethodSource;

import com.apicatalog.linkedtree.LinkedNode;
import com.apicatalog.linkedtree.LinkedTree;
import com.apicatalog.linkedtree.builder.TreeBuilderError;
import com.apicatalog.linkedtree.jsonld.JsonLdComparison;
import com.apicatalog.linkedtree.jsonld.io.JsonLdTreeReader;

import jakarta.json.Json;
import jakarta.json.JsonArray;
import jakarta.json.JsonStructure;
import jakarta.json.JsonValue;
import jakarta.json.JsonWriter;
import jakarta.json.JsonWriterFactory;
import jakarta.json.stream.JsonGenerator;

@TestMethodOrder(OrderAnnotation.class)
class NodeWriterTest {

    static JsonLdTreeReader READER = JsonLdTreeReader.generic();

    @DisplayName("Read/Write")
    @ParameterizedTest(name = "{0}")
    @MethodSource({ "resources" })
    void readWrite(String name, JsonArray input) throws TreeBuilderError {

        var tree = READER.read(input);
        assertNotNull(tree);
        
        NodeWriter.writeToStdOut(tree);

//        assertTrue(compareJson(name, tree, output, input));
    }

    static final Stream<Object[]> resources() throws IOException, URISyntaxException {
        return resources("ltd", ".jsonld");
    }

    static final Stream<Object[]> resources(String folder, String suffix) throws IOException, URISyntaxException {
        return Files.walk(Paths.get(LinkedTree.class.getResource(folder).toURI()), 1)
                .filter(name -> name.toString().endsWith(suffix))
                .sorted()
                .map(path -> {
                    try (var reader = Json.createReader(LinkedTree.class.getResourceAsStream(folder + "/" + path.getFileName().toString()))) {
                        return new Object[] { path.getFileName().toString(), reader.readArray() };
                    }
                });
    }

    static final boolean compareJson(final String testCase, final LinkedNode data, final JsonStructure result, final JsonStructure expected) {

        if (JsonLdComparison.equals(expected, result)) {
            return true;
        }

        write(testCase, result, expected, null);

        final StringWriter stringWriter = new StringWriter();
        (new NodeWriter(new PrintWriter(stringWriter))).print(data);
        System.out.print(stringWriter.toString());

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
