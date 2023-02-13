signature IntMapSig =
sig
  type 'a map 
  exception NotFound
  val apply : 'a map * int -> 'a
  val update : int * 'a * 'a map -> 'a map
end;

signature ValSig =
sig
  type value
end;

signature SymSig = 
sig
  eqtype sym 
  val hash : sym -> int
end;

signature SymTblSig = 
sig 
  structure Val: ValSig 
  structure Sym: SymSig
  type table
  exception Lookup
  val lookup : table * Sym.sym -> Val.value
  val update : table * Sym.sym * Val.value -> table
end


functor IntMap() : IntMapSig = 
  struct
    type 'a map = (int * 'a) list
    exception NotFound

    fun apply (lst , value) = 
    case lst of
    nil => raise NotFound
    | (key, a) :: tl => if value = key then a else apply(tl, value)
    
    fun update (key, update_val, lst ) = 
    case lst of 
    nil => raise NotFound
    | (key1, a)::tl => if (key = key1) then (key, update_val)::tl else (key1,a)::update(key, update_val, tl)

  end

functor ValFct() : ValSig =
  struct
    type value = int
  end

functor SymFct(): SymSig = 
  struct
    type sym = string
    fun hash (s : sym) =
      let
        val sum = ref 0
        fun loop (i : int) =
          if i >= String.size s then !sum
          else (sum := !sum + ord (String.sub (s, i)) ; loop (i + 1))
      in
        loop 0
      end
  end


functor SymTblFct(
  structure IntMap : IntMapSig
  structure Val : ValSig
  structure Sym : SymSig) : SymTblSig  =

  struct 
    structure Val = Val
    structure Sym = Sym
   
    datatype table = TBL of (Sym.sym * Val.value) list IntMap.map

    exception Lookup

    fun find (sym,[]) = raise Lookup
      | find (sym, (sym',v)::rest) =
            if sym = sym' then v
            else find (sym,rest)

    fun lookup (TBL map, s) =
        let val n = Sym.hash(s)
            val l = IntMap.apply(map,n)
        in find (s,l)
        end handle IntMap.NotFound => raise Lookup

    fun update_entry (s, v, []) = [(s, v)]
      | update_entry (s, v, (s', v')::rest) =
        if s = s' then (s, v)::rest
        else (s', v')::update_entry (s, v, rest)

    fun update (TBL map, s, v) =
      let val n = Sym.hash(s)
          val l = IntMap.apply(map, n)
          val updated_list = update_entry (s, v, l)
      in TBL (IntMap.update (n, updated_list, map))
      end 
  end
