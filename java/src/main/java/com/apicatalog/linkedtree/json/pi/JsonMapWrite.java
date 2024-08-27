package com.apicatalog.linkedtree.json.pi;

import java.util.Map;

import com.apicatalog.linkedtree.pi.ProcessingInstruction;

import jakarta.json.JsonObjectBuilder;
import jakarta.json.JsonValue;

public record JsonMapWrite(Map<String, JsonValue> value) implements ProcessingInstruction {

    public void write(JsonObjectBuilder builder) {
        value.entrySet().forEach(e -> builder.add(e.getKey(), e.getValue()));
    }

}
