# Linked Tree Format

An experiment inspired by JSON-LD's expanded and flattened forms, followed by questions such as: 
* What if we detach identifiers from nodes and limit type declarations to a single one?
* Would this help reduce processing complexity, improve speed, diagnostics, and analytics (i.e., adaptability), and could it enable faster canonicalization?
* Would that impact the design of type and definition systems?
* and What if we simplify JSON-LD context definitions by reducing their capabilities and expressivity to simple mappings? This approach could enable greater analytics and facilitate comparisons to identify differences in definitions, changes in meaning, and boost processing performance.

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
