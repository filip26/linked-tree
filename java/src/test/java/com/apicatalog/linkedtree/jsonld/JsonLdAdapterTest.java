package com.apicatalog.linkedtree.jsonld;

import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;

import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;
import java.time.Instant;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.stream.Collectors;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.MethodOrderer.OrderAnnotation;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestMethodOrder;

import com.apicatalog.linkedtree.LinkedTree;
import com.apicatalog.linkedtree.TestUtils;
import com.apicatalog.linkedtree.adapter.NodeAdapterError;
import com.apicatalog.linkedtree.builder.TreeBuilderError;
import com.apicatalog.linkedtree.jsonld.io.JsonLdTreeReader;
import com.apicatalog.linkedtree.jsonld.io.JsonLdTreeWriter;
import com.apicatalog.linkedtree.literal.ByteArrayValue;
import com.apicatalog.linkedtree.orm.mapper.TreeReaderMapping;
import com.apicatalog.linkedtree.test.AlumniCredential;
import com.apicatalog.linkedtree.test.Base64ByteArray;
import com.apicatalog.linkedtree.test.BitstringStatusListEntry;
import com.apicatalog.linkedtree.test.UnknownStatus;
import com.apicatalog.linkedtree.test.VerifiableCredential;
import com.apicatalog.linkedtree.xsd.XsdDateTime;

import jakarta.json.JsonArray;

@DisplayName("JsonLd Adapter Tests")
@TestMethodOrder(OrderAnnotation.class)
class JsonLdAdapterTest {

    static JsonLdTreeReader READER = JsonLdTreeReader.of(TreeReaderMapping.createGenericBuilder()
            // types
            .with(VerifiableCredential.typeAdapter())
            .with(AlumniCredential.typeAdapter())
            .with(BitstringStatusListEntry.TYPE,
                    BitstringStatusListEntry.class,
                    BitstringStatusListEntry::of)

            // literals
            .with(Base64ByteArray.typeAdapter())
            .with(XsdDateTime.typeAdapter())
            .build());

    static JsonLdTreeWriter WRITER = new JsonLdTreeWriter();

    @Test
    void base64ByteArray() throws IOException, URISyntaxException, TreeBuilderError, ClassCastException, NodeAdapterError {

        JsonArray input = TestUtils.resource("jsonld/custom/base64-1.jsonld");
        JsonArray output = TestUtils.resource("jsonld/custom/base64-2.jsonld");

        var tree = READER.read(input);

        assertNotNull(tree);

        var literal = tree
                .fragment()
                .literal("http://example.org/test#property4");

        assertEquals("RW5jb2RlIHRvIEJhc2U2NCBmb3JtYXQ=", literal.lexicalValue());
        
        if (literal instanceof ByteArrayValue byteArrayValue) {
            assertArrayEquals("Encode to Base64 format".getBytes(), byteArrayValue.byteArrayValue());
    
        } else {
            fail();
        }
        
        ((Base64ByteArray) literal).byteArrayValue("test X".getBytes());
        assertEquals("dGVzdCBY", literal.lexicalValue());

        JsonArray copy = WRITER.write(tree);

        assertNotNull(output);

        assertTrue(TestUtils.compareJson("", copy, output));
    }

    @Test
    void credential() throws IOException, URISyntaxException, TreeBuilderError, ClassCastException, NodeAdapterError {

        JsonArray input = TestUtils.resource("jsonld/custom/signed-vc-1.jsonld");

        LinkedTree tree = READER.read(
                List.of("https://www.w3.org/2018/credentials/v1",
                        "https://w3id.org/security/data-integrity/v2"),
                input);

        assertNotNull(tree);

        VerifiableCredential vc = tree.materialize(VerifiableCredential.class);

        assertNotNull(vc);

        assertEquals(
                URI.create("urn:uuid:58172aac-d8ba-11ed-83dd-0b3aef56cc33"),
                vc.id());

        assertEquals(new HashSet<>(Arrays.asList(new String[] {
                "https://www.w3.org/2018/credentials#VerifiableCredential",
                "https://www.w3.org/ns/credentials/examples#AlumniCredential"
        })), vc.type().stream().collect(Collectors.toSet()));

        assertEquals(1, vc.name().size());
        assertEquals("Alumni Credential", vc.name().first().lexicalValue());
        assertNull(vc.name().get("en-US"));
        assertNull(vc.name().first().language());

        assertEquals(1, vc.name().values().size());
        assertEquals(1, vc.name().languages().size());

        assertNotNull(vc.description());

        assertEquals(Instant.parse("2023-01-01T00:00:00Z"), vc.validFrom());

        assertNull(vc.validUntil());

        assertEquals(1, vc.subject().size());

        assertEquals(
                URI.create("https://vc.example/issuers/5678"),
                vc.issuer());

        AlumniCredential avc = vc.type().materialize(AlumniCredential.class);
        assertNotNull(avc);

        assertEquals(2, avc.status().size());

        var it = avc.status().iterator();

        var status1 = it.next();
        var status2 = it.next();

        assertTrue(status1 instanceof BitstringStatusListEntry);
        assertTrue(status2 instanceof UnknownStatus);
    }
}
