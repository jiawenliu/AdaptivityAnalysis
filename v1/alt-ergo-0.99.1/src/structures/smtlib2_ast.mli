(******************************************************************************)
(*     Alt-Ergo: The SMT Solver For Software Verification                     *)
(*     Copyright (C) 2013-2014 --- OCamlPro                                   *)
(*     This file is distributed under the terms of the CeCILL-C licence       *)
(******************************************************************************)

(******************************************************************************)
(*     The Alt-Ergo theorem prover                                            *)
(*     Copyright (C) 2006-2013                                                *)
(*     CNRS - INRIA - Universite Paris Sud                                    *)
(*                                                                            *)
(*     Sylvain Conchon                                                        *)
(*     Evelyne Contejean                                                      *)
(*                                                                            *)
(*     Francois Bobot                                                         *)
(*     Mohamed Iguernelala                                                    *)
(*     Stephane Lescuyer                                                      *)
(*     Alain Mebsout                                                          *)
(*                                                                            *)
(*   This file is distributed under the terms of the CeCILL-C licence         *)
(******************************************************************************)

type specconstant =
    SpecConstsDec of Loc.loc * string
  | SpecConstNum of Loc.loc * string
  | SpecConstString of Loc.loc * string
  | SpecConstsHex of Loc.loc * string
  | SpecConstsBinary of Loc.loc * string

type symbol = Symbol of Loc.loc * string | SymbolWithOr of Loc.loc * string

type sexpr =
    SexprSpecConst of Loc.loc * specconstant
  | SexprSymbol of Loc.loc * symbol
  | SexprKeyword of Loc.loc * string
  | SexprInParen of Loc.loc * (Loc.loc * sexpr list)

type attributevalue =
    AttributeValSpecConst of Loc.loc * specconstant
  | AttributeValSymbol of Loc.loc * symbol
  | AttributeValSexpr of Loc.loc * (Loc.loc * sexpr list)

type attribute =
    AttributeKeyword of Loc.loc * string
  | AttributeKeywordValue of Loc.loc * string * attributevalue

type an_option = AnOptionAttribute of Loc.loc * attribute

type infoflag = InfoFlagKeyword of Loc.loc * string

type identifier =
    IdSymbol of Loc.loc * symbol
  | IdUnderscoreSymNum of Loc.loc * symbol * (Loc.loc * string list)

type sort =
    SortIdentifier of Loc.loc * identifier
  | SortIdSortMulti of Loc.loc * identifier * (Loc.loc * sort list)

type qualidentifier =
    QualIdentifierId of Loc.loc * identifier
  | QualIdentifierAs of Loc.loc * identifier * sort

type sortedvar = SortedVarSymSort of Loc.loc * symbol * sort

type varbinding = VarBindingSymTerm of Loc.loc * symbol * term

and term =
    TermSpecConst of Loc.loc * specconstant
  | TermQualIdentifier of Loc.loc * qualidentifier
  | TermQualIdTerm of Loc.loc * qualidentifier * (Loc.loc * term list)
  | TermLetTerm of Loc.loc * (Loc.loc * varbinding list) * term
  | TermForAllTerm of Loc.loc * (Loc.loc * sortedvar list) * term
  | TermExistsTerm of Loc.loc * (Loc.loc * sortedvar list) * term
  | TermExclimationPt of Loc.loc * term * (Loc.loc * attribute list)

type command =
    CSetLogic of Loc.loc * symbol
  | CSetOption of Loc.loc * an_option
  | CSetInfo of Loc.loc * attribute
  | CDeclareSort of Loc.loc * symbol * string
  | CDefineSort of Loc.loc * symbol * (Loc.loc * symbol list) * sort
  | CDeclareFun of Loc.loc * symbol * (Loc.loc * sort list) * sort
  | CDefineFun of Loc.loc * symbol * (Loc.loc * sortedvar list) * sort * term
  | CPush of Loc.loc * string
  | CPop of Loc.loc * string
  | CAssert of Loc.loc * term
  | CCheckSat of Loc.loc
  | CGetAssert of Loc.loc
  | CGetProof of Loc.loc
  | CGetUnsatCore of Loc.loc
  | CGetValue of Loc.loc * (Loc.loc * term list)
  | CGetAssign of Loc.loc
  | CGetOption of Loc.loc * string
  | CGetInfo of Loc.loc * infoflag
  | CExit of Loc.loc

type commands = Commands of Loc.loc * (Loc.loc * command list)

val loc_an_option : an_option -> Loc.loc

val loc_attribute : attribute -> Loc.loc

val loc_attributevalue : attributevalue -> Loc.loc

val loc_command : command -> Loc.loc

val loc_commands : commands -> Loc.loc

val loc_identifier : identifier -> Loc.loc

val loc_infoflag : infoflag -> Loc.loc

val loc_qualidentifier : qualidentifier -> Loc.loc

val loc_sexpr : sexpr -> Loc.loc

val loc_sort : sort -> Loc.loc

val loc_sortedvar : sortedvar -> Loc.loc

val loc_specconstant : specconstant -> Loc.loc

val loc_symbol : symbol -> Loc.loc

val loc_term : term -> Loc.loc

val loc_varbinding : varbinding -> Loc.loc

val loc_couple : 'a * 'b -> 'a

val loc_of : commands -> Loc.loc
