# Linked Tree Format

An experiment inspired by JSON-LD's expanded and flattened forms, followed by questions such as: 

Here is a refined version for better readability and clarity:

- What if we detach identifiers from nodes and limit type declarations to a single one?  
- Would this help reduce processing complexity, boost performance, and enable faster canonicalization?  
- What if we simplify JSON-LD context definitions by reducing their expressivity to basic mappings?  
- Would this improve analytics and diagnostics (i.e., adaptability) and facilitate comparisons to identify differences in definitions and shifts in meaning?  
- How would this affect the design of type and definition systems?

The downside? LTF is a fixed, mandated format. An existing JSON or JSON-LD document must be transformed into this format using separate tools, specialized JSON-LD to LTF converters, or through declarative or programmatic custom transformations with third-party technologies.

## Processing

LinkedTree has a single form (LTF) - that's it. The LinkedTree processor provides different layers, such as a semantic layer, which follows the same structural form but offers different types of contextual information. 

This is a major distinction from JSON-LD processing, where a document is transformed into multiple forms, and the document structure is changed.

### Layers

* Base
* Sematic
* Meta ~ Statements about statements
* ??? Validation|Shape Check?
* Query? - Resource Layer, Predicate Layer
* Render

Layers can be organized in a hierarchical structure and can be composite, meaning they can combine multiple layers. For example, the Resource Layer depends directly on the base LTF layer for identifiable or referenced resources, while the Predicate Layer, optimized for querying, relies on the Semantic Layer.

## Format

### Traits

- **Top-Level Identifiable Nodes:**  
  Nodes with unique identifiers are maintained at the top level of the hierarchy, ensuring they are directly accessible.

- **Single-Type Nodes with Multiple Associations:**  
  Each node is assigned a single type; however, nodes of different types can be associated with one another.

- **Embedded Blank Nodes:**  
  Blank nodes (nodes without a unique identifier) remain embedded within their parent structures.

- **Direct Reference by Identifier:**  
  Nodes with unique identifiers can be directly referenced using their identifier.


### Outline

```typescript

// A type alias for values that can either be a URI or a Node.
type NodeOrRef = URI | Node;

// A type alias for node entries which may be a single NodeOrRef or an unordered set of them.
type NodeEntry = NodeOrRef | NodeOrRef[];

interface LinkedTree {
  // The root can be a single NodeOrRef or an unordered set of NodeOrRef.
  root: NodeOrRef | NodeOrRef[];
  
  // A mapping of identifiers to their corresponding Node objects (an unordered set).
  nodes: Map<URI, Node | Node[]>;
}

interface Node {
  // The type of the node, represented as a single URI.
  type: URI;
  
  // A mapping of terms to node entries, which can be direct node references, URIs, or arrays thereof.
  predicates: Map<URI, NodeEntry>;
}

```
### Example

#### Source JSON-LD
```javascript
{
  "@context": [
    "https://www.w3.org/ns/credentials/v2",
    "https://www.w3.org/ns/credentials/examples/v2"
  ],
  "id": "http://university.example/credentials/3732",
  "type": ["VerifiableCredential", "ExampleDegreeCredential"],
  "issuer": "https://university.example/issuers/565049",
  "validFrom": "2010-01-01T00:00:00Z",
  "credentialSubject": {
    "id": "did:example:ebfeb1f712ebc6f1c276e12ec21",
    "degree": {
      "type": "ExampleBachelorDegree",
      "name": "Bachelor of Science and Arts"
    }
  }
}
```
#### Equivalent LinkedTree

```javascript
{
  "@root": "http://university.example/credentials/3732",

  "http://university.example/credentials/3732" : [{
    "@def": "https://www.w3.org/ns/credentials/v2",
    "type": "VerifiableCredential"
    "issuer": "https://university.example/issuers/565049",
    "validFrom": "2010-01-01T00:00:00Z",
    "credentialSubject": "did:example:ebfeb1f712ebc6f1c276e12ec21"
  }, {
    "@def": "https://www.w3.org/ns/credentials/examples/v2",
    "type": "ExampleDegreeCredential"
  }],
  "did:example:ebfeb1f712ebc6f1c276e12ec21": {
    "@def": "https://www.w3.org/ns/credentials/examples/v2",
    "degree": {
      "type": "ExampleBachelorDegree",
      "name": "Bachelor of Science and Arts"
    }
  }
}

```

#### Layered LinkedTree [WiP]

LayeredLinkedTree is an illustrative example of how layers can add to the base LTF, enhancing it with specific context, such as identity, predicates, or even various render options.

```javascript
{
  "@root": "http://university.example/credentials/3732",
  "@type": "ltf:dataset",
  "http://university.example/credentials/3732" : [{
    "@type": "ltf:layer",
    "ltf:layer:base": {
      "@def": "https://www.w3.org/ns/credentials/v2",
      "type": "VerifiableCredential"
      "issuer": "https://university.example/issuers/565049",
      "validFrom": "2010-01-01T00:00:00Z",
      "credentialSubject": "did:example:ebfeb1f712ebc6f1c276e12ec21"
    },
...TODO separate lateyers/trees
  }, {
    "@def": "https://www.w3.org/ns/credentials/examples/v2",
    "type": "ExampleDegreeCredential"
  }],
  "did:example:ebfeb1f712ebc6f1c276e12ec21": {
    "@def": "https://www.w3.org/ns/credentials/examples/v2",
    "degree": {
      "type": "ExampleBachelorDegree",
      "name": "Bachelor of Science and Arts"
    }
  }
}


    "ltf:layer:semantic": {
      "type": { "@value": "@type", "@def: "" },
      "issuer": { "@value": "https://...", "@def: "" }
    
    },
    "ltf:layer:render:article:section": {
       "name": { "@type": "@title", "@default": "..."},
       "@sort": [ "issuer", "validFrom", "validUntil" ],
    }
```

### Definitions

A definition is a straightforward key-value mapping that represents a vocabulary in its simplest form-just the term and its definition, expressed as an absolute URI.

A JSON-LD document can be converted to Linked Tree Format (LTF) using a set of existing dictionaries and mapping rules. And vice versa, LTF can be converted into JSON-LD with an appropriate context or a set of contexts. The conversions are performed through the JSON-LD expanded form, to which both formats can be transformed.

### JSON Representations of Typed Value Nodes

- **Typed Value Node:**  
Represents a value with an associated type.
```javascript
{ "@value": value, "@type": URI }
```

- **Ordered List Value Node:**  
Represents a list of items with a specified order.
```javascript
{ "@value": [ items ], "@type": "@list" }
```

- **Unordered Set Value Node:**  
Represents a set of items where order does not matter.
```javascript
{ "@value": [ items ], "@type": "@set" }
```

- **Map or Sparse Array Value Node:**  
Represents a mapping of key-value pairs or a sparse array.
```javascript
{ "@value": { "key|[index]": value }, "@type": "@map" }
```

### Template

```json
{
 "id|@keyword" : [
    {
       "@type": "",
       "predicate": [
        // node reference
             "id",
        // value node - leaf
         {
           "@value": , "@type": 
       },
        // blank node
       {
           "predicate": "id"
        }

        ]
     },
     {
     }
  ],
  "@root": "id" or // blank node { ... }
  
}
```
