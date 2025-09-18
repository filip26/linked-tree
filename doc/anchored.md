# Anchored Document Form

## Definition
The **anchored form** of a JSON-LD document is a representation in which:

- all terms are fully **expanded** to absolute IRIs,
- all **named nodes** (subjects with stable identifiers) are **flattened** into a top-level node map, and
- all **blank nodes** remain **embedded** within the node object in which they occur.

## Motivation
The **anchored form** is designed for **runtime JSON-LD processing at the semantic level without dropping to RDF**.  
By flattening only named nodes, it keeps **subject-centric groupings** intact and makes it straightforward to locate, 
traverse, and transform subjects by `@id` while preserving the **local relational structure** provided by blank nodes.

Keeping blank nodes embedded:

- preserves the **contextual semantics** of anonymous structures (no skolemization required),
- enables **efficient canonicalization** via deterministic hashing of embedded subtrees (blank-node neighborhoods are contiguous and stable), and
- reduces cross-document joins during processing, since related anonymous data remains **co-located** with its parent subject.

---

The **anchored form** goal is to balances global addressability of named nodes with locality of anonymous structures, 
enabling performant, semantics-aware JSON-LD manipulation - **without** requiring an intermediate RDF phase.

```javascript
{
  @root: {   // or @root : "id" -> @subject, @graph
  
  },
  id1: [
   // grouped by @graph to keep @type compact 
   {
    @graph: id2, // or path starting with @root/ for anon graphs?????
    ...
    p1: [{
     "@list": [{
       
    }, 10, false]
     }]
  },
  id2: {
    @graph: [
       id1,
       {
       }
     ]
  },
...
}
```
