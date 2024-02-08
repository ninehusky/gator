Require Import Utf8.


Require Import Arith Lia List String.
Import ListNotations.
Open Scope string.

(* infer type arguments *)
Set Implicit Arguments.

Inductive OpType : Set :=
  | Plus.

Definition Id : nat.
(* var, time -> val *)
Definition env := string -> nat -> nat.
Definition time := nat.

Inductive node : Set :=
  | BV (b : nat)
  | Var (s : string)
  | Op (o : OpType) (e1 : nat) (e2 : nat)
  | Reg (regid : nat) (init : nat).
  
Definition prog := list (node).

(* Ltac --> interpreter, starter code for some homework thing in 505 *)
(* index is a function that takes an id and a program, and returns the node at p[i] *)
Fixpoint index (i : nat) (p : prog) : node :=
  match (i, p) with
    | (_, []) => (Var "error")
    | (0, (n :: _)) => n
    | (S n, p) => index n (tl p)
  end.

(* explore writing eval as inductive data *)

(* step : prog -> env -> id -> time -> bv -> Prop *)
Inductive step : prog -> env -> nat -> time -> nat -> Prop :=
| step_bv : forall p env id t b,
    step p env id t b
| step_var : forall p env id t x,
    step p env id t (env x t)
| step_op : forall p env id t e1 e2 v1 v2,  
    step p env e1 t v1 ->
    step p env e2 t v2 ->
    step p env id t (v1 + v2) 
| step_reg : forall p env id t n init, 
    step p env n t init ->
    (t = 0 -> step p env id t init \/ t > 0 -> step p env id (t - 1) 0).

Lemma check_simple_comb_output :
  let prog := [(Op Plus 1 2) ; (BV 1) ; (BV 2)] in
  let env    := (fun x y => 0) in
  let id_num := 0 in
  forall t,
    step prog env id_num t 3.
Proof.
  intros.
  unfold prog.
  unfold env.
  unfold id_num.
  apply step_op with (e1 := 1) (e2 := 2) (v1 := 1) (v2 := 2).
  - constructor.
  - apply step_bv.
  Qed.


