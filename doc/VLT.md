# Vectorized Processing of Linked Trees 

```
// term node
[ parent, first child, next sibling, prev sibling, term, _]
```

```
// value node
[ parent,  next sibling, prev sibling, declared type, native type, value]
```

```
// free node
[ _, next sibling, _, _, _, _]
```

```
// root 
[ _,  first child, first free node, last free node, capacity]
```

```javascript

tree.allocate(term);
tree.allocate(declared type, native type, value);

tree.link(parent, prev, node);

tree.insert(parent, prev, term);
tree.insert(parent, prev, declared type, native type, value);

tree.insert(node, term);
tree.update(node, declared type, native type, value);

tree.remove(node);

// traversal
tree.childs(node);
tree.next(node);
tree.prev(node);
tree.parent(node);
tree.get(index);
```
