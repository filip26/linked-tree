package com.apicatalog.linkedtree.json.pi;

import java.util.Map;

import com.apicatalog.linkedtree.pi.ProcessingInstruction;

import jakarta.json.JsonObjectBuilder;
import jakarta.json.JsonValue;

public record JsonObjectWrite(
        Map<String, JsonValue> jsonObject) implements ProcessingInstruction {

    public void write(JsonObjectBuilder builder) {
        jsonObject.entrySet().forEach(e -> builder.add(e.getKey(), e.getValue()));
    }

}
