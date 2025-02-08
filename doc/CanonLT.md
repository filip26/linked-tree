# Canonical Linked Tree Format

- **Top-Level Identifiable Nodes:**  
  Nodes with unique identifiers are maintained at the top level of the hierarchy, ensuring they are directly accessible.

- **Single-Type Nodes with Multiple Associations:**  
  Each node is assigned a single type; however, nodes of different types can be associated with one another.

- **Embedded Blank Nodes:**  
  Blank nodes (nodes without a unique identifier) remain embedded within their parent structures.

- **Direct Reference by Identifier:**  
  Nodes with unique identifiers can be directly referenced using their identifier.

```typescript

// A type alias for values that can either be a URI or a Node.
type NodeOrRef = URI | Node;

// A type alias for node entries which may be a single NodeOrRef or an array of them.
type NodeEntry = NodeOrRef | NodeOrRef[];

interface LinkedTree {
  // The root can be a single NodeOrRef or an array of NodeOrRef.
  root: NodeOrRef | NodeOrRef[];
  
  // A mapping of identifiers to their corresponding Node objects.
  nodes: Map<URI, CNLTNode | Node[]>;
}

interface Node {
  // The type of the node, represented as a single URI.
  type: URI;
  
  // A mapping of terms to node entries, which can be direct node references, URIs, or arrays thereof.
  predicates: Map<URI, NodeEntry>;
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
