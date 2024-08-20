package com.apicatalog.linkedtree.jakarta;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

import java.io.IOException;
import java.net.URISyntaxException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.stream.Stream;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.MethodOrderer.OrderAnnotation;
import org.junit.jupiter.api.TestMethodOrder;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.MethodSource;

import com.apicatalog.linkedtree.LinkedFragment;

import jakarta.json.Json;
import jakarta.json.JsonArray;

@DisplayName("Jakarta Test Suite")
@TestMethodOrder(OrderAnnotation.class)
class JakartaTest {

    static JakartaLinkedTreeReader JAKARTA = new JakartaLinkedTreeReader();
    
    @DisplayName("Read/Write")
    @ParameterizedTest(name = "{0}")
    @MethodSource({ "expandedResources" })
    void readWrite(String name, JsonArray input) {
    
        var tree = JAKARTA.read(input);
        
        assertNotNull(tree);

        System.out.println(tree);
//        var output = JAKARTA.write(tree);
//        
//        assertNotNull(output);

//        assertEquals(input, output);
    }

    static final Stream<Object[]> expandedResources() throws IOException, URISyntaxException {
        return Files.walk(Paths.get(LinkedFragment.class.getResource("").toURI()), 1)
                .filter(name -> name.toString().endsWith("out.jsonld"))
                .sorted()
                .map(path -> {
                    try (var reader = Json.createReader(LinkedFragment.class.getResourceAsStream(path.getFileName().toString()))) {
                        return new Object[] { path.getFileName().toString(), reader.readArray() };
                    }
                });
    }
}
