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
[ first child, first free node, capacity]
```

```javascript
tree.insert(parent, prev, declared type, native type, value);

tree.remove(node);

// traversal
tree.childs(node);
tree.next(node);
tree.prev(node);
tree.parent(node);
tree.get(index);
```
