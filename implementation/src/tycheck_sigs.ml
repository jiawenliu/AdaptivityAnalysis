module type CH_TYPE =
    sig
      type ty
    end

 

module type CHECK =
    sig
      open IndexSyntax
      open Constr
      open Ctx
      open Support.FileInfo
      open DMap

      type ty
      type cost = iterm option

      type 'a ty_error = 
          Right of 'a
        | Left  of Ty_error.ty_error_elem Support.FileInfo.withinfo


      type dmap = DMap.dmap

      type adapt = iterm



      (* Reader/Error monad for type-checking *)
      type 'a checker = ty context -> ('a * dmap * adapt) ty_error
          
      (* Reader/Error monad for type-inference *)
      type 'a inferer =  ty context -> ('a * constr * dmap * adapt) ty_error
   
      (* Reader/Error monad for Equivalent-checking *)
      type 'a equiv_checker = ty context -> 'a ty_error

      val (<=<) : constr equiv_checker -> constr -> constr equiv_checker
      val (<<) : constr equiv_checker -> constr equiv_checker -> constr equiv_checker
      val (>>=) : 'a checker -> ('a -> 'b checker) -> 'b checker
      val (>>>=) : 'a checker -> ('a -> 'b checker) -> 'b checker
      val (>>)   : constr checker -> constr checker -> constr checker
      val (>&&>)  : constr checker -> constr checker -> constr checker
      val (>||>)  : constr checker -> constr checker -> constr checker
      val (<<=)  : 'a inferer -> ('a -> 'b inferer) -> 'b inferer
      val (<->=) : ty inferer -> (ty -> (constr checker * dterm * var_info)) -> constr checker
      val (=<->) : ty inferer-> (ty -> (constr checker * ty * iterm * dmap * iterm) ) -> ty inferer      
      val return_ch     : constr -> 'a * 'b  -> constr ty_error
      val return_inf    : 'a -> 'a inferer
      val return_leaf_ch : constr checker
      val return_eq_ch : 'a -> 'a equiv_checker
      val return_leaf_eq_ch : 'a equiv_checker
      val fail : Ty_error.ty_error_elem -> 'a -> 'b ty_error

      val get_infer_ctx  : ty context inferer
      val get_var_ty     : var_info -> ty inferer

      val with_new_ctx : (ty context -> ty context) -> 'a checker -> 'a checker
     
      (* Extend the type context with a fresh type variable binding, for checker *)
      val (|:|) : var_info -> ty -> constr checker -> constr checker
      (*val (|:-|) : var_info -> ty -> constr checker -> constr checker*)

      (* Extend the sort context with a fresh index variable binding, for checker *)
      val (|::|) : var_info -> sort -> constr checker -> constr checker

      (* Extend the existential context with a fresh index variable binding, for checker *)
(*      val (|:::|) : var_info -> sort -> info -> constr checker -> constr checker

       val (|::::|) : var_info -> sort -> info -> constr checker -> constr checker
*)
      (* Change checking mode *)
      (*val with_mode : Syntax.mode -> 'a checker -> 'a checker*)
      
      val check_size_eq : iterm -> iterm -> constr checker -> constr checker
      val assume_size_eq : iterm -> iterm -> constr checker -> constr checker
      val assume_size_leq : iterm -> iterm -> constr checker -> constr checker
      val check_size_leq : iterm -> iterm -> constr checker -> constr checker
      
      val cost_cs : 'a context -> iterm * iterm -> constr
      val cost_cs_st : 'a context -> iterm * iterm -> constr

     (*  val check_predicate : predicate -> predicate -> bool
      val check_arrays : iterm -> iterm -> bool *)

    end
 
