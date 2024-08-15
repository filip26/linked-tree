package com.apicatalog.linkedtree;

import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Iterator;
import java.util.stream.Stream;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.MethodOrderer.OrderAnnotation;
import org.junit.jupiter.api.TestMethodOrder;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.MethodSource;

import jakarta.json.Json;
import jakarta.json.JsonArray;

@DisplayName("Jakarta Test Suite")
@TestMethodOrder(OrderAnnotation.class)
class JakartaTest {

    @DisplayName("Read/Write")
    @ParameterizedTest(name = "{0}")
    @MethodSource({ "expandedResources" })
    void readWrite(JsonArray input) {
        System.out.println(input);
    }

    static final Stream<JsonArray> expandedResources() throws IOException, URISyntaxException {
        return Files.walk(Paths.get(JakartaTest.class.getResource("").toURI()), 1)
                .filter(name -> name.toString().endsWith("out.jsonld"))
                .map(path -> {
                    try (var reader = Json.createReader(JakartaTest.class.getResourceAsStream(path.getFileName().toString()))) {
                        return reader.readArray();
                    }
                });
    }
}
