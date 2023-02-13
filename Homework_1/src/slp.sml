type id = string

datatype binop = Plus | Minus | Times | Div

datatype stm = CompoundStm of stm * stm
	              | AssignStm of id * exp
	              | PrintStm of exp list

          and exp = IdExp of id
                | NumExp of int
                | OpExp of exp * binop * exp
                | EseqExp of stm * exp

val prog1 = 
 CompoundStm(AssignStm("a",OpExp(NumExp 5, Plus, NumExp 3)),
  CompoundStm(AssignStm("b",
      EseqExp(PrintStm[IdExp"a",OpExp(IdExp"a", Minus,NumExp 1)],
           OpExp(NumExp 10, Times, IdExp"a"))),
   PrintStm[IdExp "b"]))


val prog2 = CompoundStm(AssignStm("a",OpExp(NumExp 5, Plus, NumExp 3)),
  CompoundStm(AssignStm("b",
      EseqExp(PrintStm[IdExp"a",EseqExp(PrintStm [IdExp "a", 
                                                  IdExp "a", 
                                                  IdExp "a"],
                                        OpExp(IdExp"a", Minus,NumExp 1))],
           OpExp(NumExp 10, Times, IdExp"a"))),
   PrintStm[IdExp "b"]))

val prog3 = 
 CompoundStm(AssignStm("a",OpExp(NumExp 5, Plus, NumExp 3)),
  CompoundStm(AssignStm("b",
      EseqExp(PrintStm[IdExp"a",EseqExp(AssignStm("a",
                                                  OpExp(IdExp "a", 
                                                        Times, 
                                                        NumExp 5)),
                                        OpExp(IdExp"a", Minus,NumExp 1))],
           OpExp(NumExp 10, Times, IdExp"a"))),
   PrintStm[IdExp "b"]))

val prog4 =
  CompoundStm(
    AssignStm("a",OpExp(NumExp 5, Plus, NumExp 3)),
    CompoundStm(AssignStm("b",
                EseqExp(PrintStm[IdExp"a",
                                 EseqExp(PrintStm [IdExp "a",
                                                   IdExp "a",
                                                   IdExp "a"],
                                         OpExp(IdExp"a", Minus,NumExp 1)),
                                 EseqExp(PrintStm[NumExp 4,
                                                  NumExp 4,
                                                  NumExp 4,
                                                  NumExp 4
                                                 ],
                                         OpExp( IdExp "a", Minus, NumExp 3))],
                        OpExp(NumExp 10, Times, IdExp"a"))),
                PrintStm[IdExp "b"]))	    
	    

(* --Solution for maxargs begin-- *)
fun max (a : int , b : int ) : int = if a > b then a else b

(* get the lenghth of a list *)
fun get_len nil = 0 
  | get_len (hd::tl) = 1 + get_len tl 

(* traverse the coumpound statement, define the mutually recursive function to return the number of max args *)
fun maxargs_stm ( CompoundStm(s1, s2) : stm ) : int = max (maxargs_stm(s1), maxargs_stm(s2))
  | maxargs_stm ( AssignStm(id1, exp1) : stm ) : int = maxargs_exp(exp1)
  | maxargs_stm ( PrintStm(exp_list) : stm ) : int = max (maxargs_explist(exp_list), get_len(exp_list))
  and maxargs_explist (nil) : int = 0
  | maxargs_explist(hd::tl) = max (maxargs_exp(hd), maxargs_explist(tl))
  and maxargs_exp (IdExp(id): exp ) : int  = 0
  | maxargs_exp(NumExp(num) : exp ) : int = 0
  | maxargs_exp(OpExp(exp1, binop1, exp2) : exp) = max(maxargs_exp(exp1), maxargs_exp(exp2))
  | maxargs_exp(EseqExp(stm1, exp1) : exp) = max (maxargs_stm(stm1), maxargs_exp(exp1)) 

val progmax_1 = maxargs_stm(prog1)
val progmax_2 = maxargs_stm(prog2)
val progmax_3 = maxargs_stm(prog3)
(* --Solution for maxargs end-- *)


(* --Solution for interp begin--*)
exception LookUp

type table = (id * int) list 

fun lookup nil id = 
     (print "Error: undefined id "; print id; print "\n"; 
      raise LookUp)
  | lookup ((n,v)::t) id =
       if n = id then v else lookup t id

fun update nil id v = [(id,v)]
  | update ((id',v')::t) id v =
       if id = id' then (id,v) :: t
       else (id',v') :: update t id v


fun interp (stm) : table = 
  let 
    fun interp_stm(CompoundStm (s1, s2), input_table) : table = let val mid_table = interp_stm(s1, input_table) in interp_stm(s2, mid_table) end
    |   interp_stm (AssignStm (id1, exp1), input_table ) : table = 
        let 
            val tuple1 = interp_exp(exp1, input_table);
            val value  = #1 tuple1;
        in 
            update input_table id1 value
        end
    |   interp_stm (PrintStm (exp_list), input_table ) : table =  
        let 
          fun print_intlist nil = ()
          | print_intlist (hd::tl) = ( print(Int.toString(hd) ^ " "); print_intlist tl)
          fun interp_explist(nil,input_table) = (nil, input_table)
          | interp_explist((hd::tl),input_table)= 
              let 
                val temp_tuple = interp_exp(hd, input_table);
                val value = #1 temp_tuple;
                val temp_table = #2 temp_tuple;
              in
                let 
                  val tuple1 = interp_explist(tl, temp_table);
                  val return_list = #1 tuple1;
                  val return_table = #2 tuple1;
                in
                  (value::return_list, return_table)
                end
              end
            val tuple1 = interp_explist(exp_list, input_table);
            val return_list = #1 tuple1;
            val return_table = #2 tuple1;
        in
          (print_intlist(return_list); print("\n"); return_table)
        end
    
    (* return an int result and a table generated in expressioins like Esexp *)
    and interp_exp (IdExp (id) , input_table) : int * table = let val value = lookup input_table id in (value, input_table) end
    |   interp_exp (NumExp(num) , input_table ) : int * table = (num, input_table)
    |   interp_exp (EseqExp(stm1, exp1) , input_table ) : int * table = 
          let 
            val temp_table = interp_stm(stm1, input_table);
            val temp_tuple = interp_exp(exp1, temp_table) ;
            val value = #1 temp_tuple;
          in 
            (value, temp_table)
          end
    |   interp_exp (OpExp(op1, binop, op2) , input_table ) : int * table = 
        let 
          val tupel1 = interp_exp(op1, input_table);
          val tupel2 = interp_exp(op2, input_table);
          val value1 = #1 tupel1;
          val value2 = #1 tupel2;
        in
          case binop of
            Plus => (value1 + value2, input_table)
          | Minus => (value1 - value2, input_table)
          | Times => (value1 * value2, input_table)
          | Div => (value1 div value2, input_table)  
        end
  in 
    let 
      val final_table = interp_stm(stm, nil)
    in 
      final_table
    end
  end

val prog1_result = interp(prog1)
val prog2_result = interp(prog2)
val prog3_result = interp(prog3)
val prog4_result = interp(prog4)
(* --Solution for interp end --*)


