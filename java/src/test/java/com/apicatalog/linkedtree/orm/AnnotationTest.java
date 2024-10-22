package com.apicatalog.linkedtree.orm;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;
import java.time.Instant;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.stream.Collectors;

import org.junit.jupiter.api.Test;

import com.apicatalog.linkedtree.Base64ByteArray;
import com.apicatalog.linkedtree.BitstringStatusListEntry;
import com.apicatalog.linkedtree.Linkable;
import com.apicatalog.linkedtree.LinkedFragment;
import com.apicatalog.linkedtree.TestUtils;
import com.apicatalog.linkedtree.adapter.NodeAdapterError;
import com.apicatalog.linkedtree.builder.TreeBuilderError;
import com.apicatalog.linkedtree.jsonld.io.JsonLdTreeReader;
import com.apicatalog.linkedtree.literal.ImmutableLiteral;
import com.apicatalog.linkedtree.orm.mapper.TreeMapping;
import com.apicatalog.linkedtree.xsd.XsdDateTime;

import jakarta.json.JsonArray;

class AnnotationTest {

    @Test
    void credential() throws IOException, URISyntaxException, TreeBuilderError, NodeAdapterError {

        TreeMapping mapping = TreeMapping.createBuilder()
                .with(BitstringStatusListEntry.TYPE,
                        BitstringStatusListEntry.class,
                        BitstringStatusListEntry::of)

                // literals
                .with(Base64ByteArray.typeAdapter())
                .with(XsdDateTime.typeAdapter())

                .scan(AnnotatedCredential.class)
                .scan(ExtendedAnnotatedCredential.class)

                .build();

        JsonArray input = TestUtils.resource("jsonld/custom/signed-vc-1.jsonld");

        JsonLdTreeReader reader = JsonLdTreeReader.of(mapping);

        AnnotatedCredential vc = reader.read(
                AnnotatedCredential.class,
                List.of("https://www.w3.org/2018/credentials/v1",
                        "https://w3id.org/security/data-integrity/v2"),
                input);

        assertVc(vc);

        ExtendedAnnotatedCredential eac = vc.type().materialize(ExtendedAnnotatedCredential.class);
        assertVc(eac);

        assertEquals(2, eac.status().size());

        var it = eac.status().iterator();

        var status1 = it.next();
        var status2 = it.next();

        assertTrue(status1 instanceof BitstringStatusListEntry);
        assertFalse(status2 instanceof BitstringStatusListEntry);

    }

    @Test
    void controller() throws IOException, URISyntaxException, TreeBuilderError, NodeAdapterError {

        TreeMapping mapping = TreeMapping.createBuilder()
                .map(ImmutableLiteral.class,
                        EncodedKey.class,
                        EncodedKeyAdapter::map)
                .scan(Multikey.class)
                .scan(ControllerDocument.class)
                .build();

        JsonLdTreeReader reader = JsonLdTreeReader.of(mapping);

        JsonArray input = TestUtils.resource("jsonld/custom/controller-doc-2.jsonld");

        ControllerDocument doc = reader.read(
                ControllerDocument.class,
                List.of("https://www.w3.org/ns/controller/v1"),
                input);

        assertNotNull(doc);
        assertEquals(URI.create("https://controller.example"), doc.id());

        assertTrue(doc.type().isEmpty());

        assertEquals(1, doc.controller().size());
        assertEquals(URI.create("https://controllerB.example/abc"), doc.controller().iterator().next());

        assertEquals(2, doc.authentication().size());
        var ait = doc.authentication().iterator();
        assertMethodK1(ait.next());

        assertEquals(2, doc.verificationMethod().size());
        var vmit = doc.verificationMethod().iterator();
        vmit.next();
        assertMethodK1(vmit.next());

        assertEquals(2, doc.assertionMethod().size());
        var amit = doc.assertionMethod().iterator();
        assertMethodK1(amit.next());

        assertEquals(0, doc.alsoKnownAs().size());
        assertEquals(0, doc.keyAgreement().size());
        assertEquals(0, doc.capabilityDelegation().size());
        assertEquals(0, doc.capabilityInvocation().size());
    }

    static void assertMethodK1(VerificationMethod method) throws NodeAdapterError {
        assertNotNull(method);
        assertEquals(URI.create("https://controller.example/123456789abcdefghi#keys-1"), method.id());
        assertTrue(method.type().contains("https://w3id.org/security#Multikey"));
        assertTrue(method instanceof Multikey);
        assertEquals("z6MkmM42vxfqZQsv4ehtTjFFxQ4sQKS2w6WR7emozFAn5cxu", ((Multikey) method).publicKey().encodedKey());
        assertNull(((Multikey) method).privateKey());
    }

    static void assertVc(AnnotatedCredential vc) {
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
        assertNotNull(((Linkable) vc).ld());
        assertTrue(((Linkable) vc).ld() instanceof LinkedFragment);

        assertEquals(URI.create("did:example:abcdefgh"), vc.subject().id());
        assertTrue(vc.subject().type().isEmpty());

        if (vc.subject() instanceof AlumniSubject asub) {
            assertEquals("The School of Examples", asub.alumniOf());
        }

    }
}
