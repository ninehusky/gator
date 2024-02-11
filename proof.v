Require Import Utf8.


Require Import Arith Lia List String.
Import ListNotations.
Open Scope string.

(* infer type arguments *)
Set Implicit Arguments.

Inductive OpType : Set :=
  | Plus.

Definition Id := nat.
(* var, time -> val *)
Definition env := string -> nat -> nat.
Definition time := nat.

Inductive node : Set :=
  | BV (b : nat)
  | Var (s : string)
  | Op (o : OpType) (e1 : Id) (e2 : Id)
  | Reg (regid : nat) (init : nat).
  
Definition prog := list (node).

(* Ltac --> interpreter, starter code for some homework thing in 505 *)
(* index is a function that takes an id and a program, and returns the node at p[i] *)
Fixpoint index (i : nat) (p : prog) : option node :=
  match (i, p) with
    | (_, []) => None
    | (0, (n :: _)) => (Some n)
    | (S n, p) => index n (tl p)
  end.

(* explore writing eval as inductive data *)

(* step : prog -> env -> id -> time -> bv -> Prop *)
Inductive step : prog -> env -> nat -> time -> nat -> Prop :=
| step_bv :
  forall p env id t b,
    index id p = Some (BV b) ->
      step p env id t b
| step_var : forall p env id t x,
    index id p = Some (Var x) ->
      step p env id t (env x t)
| step_op : forall p env id t e1 e2 v1 v2,  
    index id p = Some (Op Plus e1 e2) ->
    step p env e1 t v1 ->
    step p env e2 t v2 ->
    step p env id t (v1 + v2) 
| step_reg_init : forall p env id t n init, 
    t = 0 ->
    index id p = Some (Reg n init) ->
    step p env id t init
| step_reg : forall p env id t n init b, 
    index id p = Some (Reg n init) ->
    t > 0 ->
    step p env n t b ->
    step p env id t b.

Lemma check_simple_comb_output :
  let prog := [(Op Plus 1 2) ; (BV 1) ; (BV 2)] in
  let env    := (fun x y => 0) in
  let id_num := 0 in
  forall t,
    step prog env id_num t 3.
Proof.
  intros.
  unfold prog0.
  unfold env.
  unfold id_num.
  apply step_op with (e1 := 1) (e2 := 2) (v1 := 1) (v2 := 2).
  - constructor.
  - apply step_bv. unfold index. simpl. reflexivity.
  - apply step_bv. unfold index. simpl. reflexivity.
Qed.

Lemma simple_equiv_check :
  let prog1 := [(Reg 1 0) ; (Op Plus 2 3) ; (BV 1) ; (BV 2)] in
  let prog2 := [(Op Plus 1 2) ; (Reg 3 0) ; (Reg 4 0) ; (BV 1) ; (BV 2)] in
  let id_num := 0 in
  forall env t,
    t > 1 ->
    exists b, step prog1 env id_num t b /\ step prog2 env id_num t b.
Proof.
  intros. 
  eexists ?[b].
  unfold id_num.
  econstructor.
  - apply step_reg with (n := 1) (init := 0) (b := 3).
    + simpl. reflexivity.
    + lia.
    + unfold prog1. apply step_op with (p := prog1) (env := env0) (t := t) (id := 1) (e1 := 2) (e2 := 3) (v1 := 1) (v2 := 2).
     * simpl. reflexivity.
     * econstructor. simpl. reflexivity.
     * econstructor. simpl. reflexivity.
  - apply step_op with (p := prog2) (env := env0) (t := t) (id := 0) (e1 := 1) (e2 := 2) (v1 := 1) (v2 := 2).
    + simpl. reflexivity.
    + apply step_reg with (p := prog2) (env := env0) (t := t) (n := 3) (init := 0) (b := 1).
      * simpl. reflexivity.
      * lia.
      * econstructor. simpl. reflexivity.
    + apply step_reg with (p := prog2) (env := env0) (t := t) (n := 4) (init := 0) (b := 2).
      * simpl. reflexivity.
      * lia.
      * econstructor. simpl. reflexivity.
  Qed.

