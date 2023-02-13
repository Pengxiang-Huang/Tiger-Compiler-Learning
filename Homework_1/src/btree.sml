signature ITEM =
sig
  datatype 'a node  = Node of int * 'a
  val less_than : 'a node * 'a node -> bool 
  val equal : 'a node * 'a node -> bool
  val print_node : 'a node -> unit
end ;

signature BTREE =
sig
  structure Item : ITEM
  datatype 'a btree =  Empty | Tree of 'a Item.node * 'a btree * 'a btree
  exception Not_found
  val init_tree : 'a -> 'a btree
  val insert : 'a btree * 'a Item.node -> 'a btree
  val find : 'a btree * 'a Item.node -> 'a Item.node
  val print_tree : 'a btree -> unit
end ;


functor Item () : ITEM =
struct
  datatype 'a node  = Node of int * 'a
  fun less_than (Node(i, _), Node(j, _)) = i < j
  fun equal (Node(i, _), Node (j, _)) = i = j 
  fun print_node (Node (i, _)) = print (Int.toString i)
end ;


functor BTree (Item : ITEM) : BTREE =
struct
  structure Item = Item
  datatype 'a btree =  Empty | Tree of 'a Item.node * 'a btree * 'a btree
  exception Not_found

  fun init_tree x  = Empty
  
  (* given data item, node, return it if found, else raise error and return itself *)
  fun find (Empty, node ) = raise Not_found 
  | find (Tree (n, l, r), node) = 
        if Item.equal(n, node) then n 
        else if Item.less_than(n, node)  then find (r, node)
        else find (l, node)
  handle Not_found => (print("The Searching Item Not found\n"); node)


  fun insert (Empty, node) = Tree (node, Empty, Empty)
  | insert (Tree (n, l, r), node) = 
        if Item.equal(n, node) then Tree (node, l, r)
        else if Item.less_than(n,node) then Tree (n, l, insert (r, node))
        else Tree (n, insert (l, node), r)

  fun print_tree(Empty) = ()
  | print_tree (Tree (n, l, r)) = 
    let
      val left = print_tree(l); 
      val side_effect = Item.print_node n;
      val right = print_tree(r);
    in 
      ()
    end
end 


structure Item = Item();
structure BTree = BTree(Item);

val inittree = BTree.init_tree("init") ;

val tree = BTree.insert(inittree, Item.Node(1, "one"));;
val tree = BTree.insert(tree, Item.Node(5, "five"));
val tree = BTree.insert(tree, Item.Node(10, "ten"));
val tree = BTree.insert(tree, Item.Node(6, "six"));;
val tree = BTree.insert(tree, Item.Node(9, "nine"));
val tree = BTree.insert(tree, Item.Node(4, "four"))
val tree = BTree.insert(tree, Item.Node(2, "two"));
val tree = BTree.insert(tree, Item.Node(7, "seven"));
val tree = BTree.insert(tree, Item.Node(8, "eight"))
val tree = BTree.insert(tree, Item.Node(3, "three"));

BTree.print_tree(tree);

val found = BTree.find(tree, Item.Node(5, "five"));
(* val Not_found = BTree.find(tree, Item.Node(11, "eleven")); *)




