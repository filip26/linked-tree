package com.apicatalog.linkedtree;

import static org.junit.jupiter.api.Assertions.fail;

import java.io.IOException;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.net.URISyntaxException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.Collections;
import java.util.stream.Stream;

import com.apicatalog.linkedtree.jsonld.JsonLdComparison;

import jakarta.json.Json;
import jakarta.json.JsonArray;
import jakarta.json.JsonStructure;
import jakarta.json.JsonValue;
import jakarta.json.JsonWriter;
import jakarta.json.JsonWriterFactory;
import jakarta.json.stream.JsonGenerator;

public class TestUtils {

    public static final JsonArray resource(String name) throws IOException, URISyntaxException {
        try (var reader = Json.createReader(TestUtils.class.getResourceAsStream(name))) {
            return reader.readArray();
        }
    }

    public static final Stream<Object[]> resources(String folder, String suffix) throws IOException, URISyntaxException {
        return Files.walk(Paths.get(TestUtils.class.getResource(folder).toURI()), 1)
                .filter(name -> name.toString().endsWith(suffix))
                .sorted()
                .map(path -> {
                    try (var reader = Json.createReader(TestUtils.class.getResourceAsStream(folder + "/" + path.getFileName().toString()))) {
                        return new Object[] { path.getFileName().toString(), reader.read() };
                    }
                });
    }

    public static final Stream<Object[]> resources(String folder, String inSuffix, String outSuffix) throws IOException, URISyntaxException {
        return Files.walk(Paths.get(TestUtils.class.getResource(folder).toURI()), 1)
                .filter(name -> name.toString().endsWith(inSuffix))
                .sorted()
                .map(path -> {
                    try (var inReader = Json.createReader(TestUtils.class.getResourceAsStream(folder + "/" + path.getFileName().toString()))) {
                        try (var outReader = Json.createReader(TestUtils.class.getResourceAsStream(folder + "/" + path.getFileName().toString().replace(inSuffix, outSuffix)))) {
                            return new Object[] { path.getFileName().toString(), inReader.read(), outReader.read() };
                        }
                    }
                });
    }

    public static final boolean compareJson(final String testCase, final JsonStructure result, final JsonStructure expected) {

        if (JsonLdComparison.equals(expected, result)) {
            return true;
        }

        print(testCase, result, expected, null);

        fail("Expected " + expected + ", but was" + result);
        return false;
    }

    public static final boolean compareJson(final String testCase, final LinkedNode data, final JsonStructure result, final JsonStructure expected) {

        if (JsonLdComparison.equals(expected, result)) {
            return true;
        }

        print(testCase, result, expected, null);

        final StringWriter stringWriter = new StringWriter();
//        (new DictionaryWriter(new PrintWriter(stringWriter))).print(data);
        System.out.print(stringWriter.toString());

        fail("Expected " + expected + ", but was" + result);
        return false;
    }

    public static void print(final String testCase, final JsonStructure result, final JsonStructure expected, Exception error) {
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

    public static void prettyPrint(final JsonStructure result) {
        final StringWriter stringWriter = new StringWriter();

        try (final PrintWriter writer = new PrintWriter(stringWriter)) {

            final JsonWriterFactory writerFactory = Json.createWriterFactory(Collections.singletonMap(JsonGenerator.PRETTY_PRINTING, true));
            write(writer, writerFactory, null, result);
        }

        System.out.println(stringWriter.toString());
    }

    static final void write(final PrintWriter writer, final JsonWriterFactory writerFactory, final String name, final JsonValue result) {

        if (name != null) {
            writer.println(name + ":");
        }

        final StringWriter out = new StringWriter();

        try (final JsonWriter jsonWriter = writerFactory.createWriter(out)) {
            jsonWriter.write(result);
        }

        writer.write(out.toString());
        writer.println();
    }

}
