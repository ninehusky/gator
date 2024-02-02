Require Import Utf8.

Require Import Arith Lia List String.
Import ListNotations.
Open Scope string.

(* infer type arguments *)
Set Implicit Arguments.

Inductive OpType : Set :=
  | Plus.

Inductive ProgId : Set :=
  | Id (id_num : nat) (w : nat).

Inductive node : Set :=
  | BV (b : nat)
  | Var (x : nat) 
  | Op (o : OpType) (e1 : ProgId) (e2 : ProgId)
  | Reg (regid : ProgId) (init : nat).

(* type prog is a list of pairs with type (ProgId, node) *)
Definition prog := list (ProgId * node).

(* index is a function that takes an id and a program, and returns the node at p[i] *)
Fixpoint index (i : nat) (p : prog) : (ProgId * node) :=
  match p with
    | nil => (Id 123 0, BV 123)
    | (Id id_num w, n) :: p' => if Nat.eqb i id_num then (Id id_num w, n) else index i p'
  end.

(*
  A program is well-formed iff:
  1. p.root \in p.ids                                                       (I think we get this for free because lists)
  2. All ids are unique and distinct                                        (I think we get this for free because lists?)
  3. The inputs of all nodes in p are ids of other nodes in p               (Need to check for this? Not sure how it impacts termination of eval)
  4. All primitive nodes contain well-formed programs                       (Irrelevant)
  5. All primitive nodes bind exactly their free variables                  (Irrelevant)
  6. Program p is free of combinational loops.                              
 *)
(* eval_prog is a function that takes a program, an environment, a time, and a node, and returns a natural number *)
Fixpoint eval_prog (p : prog) (env : nat -> nat -> nat) (n : node) (t : nat) (w : nat) : nat :=
    match n with
      | BV b => b
      | Var x => env x t
      | Op Plus (Id id1 w1) (Id id2 w2) => eval_prog p env (snd (index id1 p)) t + eval_prog p env t (snd (index id2 p))
      | (Reg (Id n' w) init) => if Nat.eqb t 0 then init else eval_prog p env (t - 1) (snd (index n' p))
    end.

(* Fixpoint eval (e : node) (env : nat -> nat -> nat) (w : nat) (t : nat) : nat :=
  match e with
  | BV b => b
  | Var x => env x t
  | Op Plus e1 e2 => (assert )eval e1 env t + eval e2 env t 
  | Reg n init => if Nat.eqb t 0 then init else eval n env (t - 1)
  end.

Definition prog1 : node :=
  (Reg (Op Plus (Var 1) (Var 2)) 0).

Definition prog2 : node :=
  (Op Plus (Reg (Var 1) 0) (Reg (Var 2) 0)).

Lemma eval_prog1 : forall env t,
  t > 1 ->
  eval prog1 env t = eval prog2 env t.
Proof.
  intros env.
  intros t.
  intros H.
  simpl.
  destruct t.
  - inversion H.
  - simpl. reflexivity. 
  Qed.

  (* induction t.
  - intros. inversion H.
  - intros.
    simpl.
    destruct t.
    + simpl. reflexivity.
    + simpl. reflexivity.
Qed. *)
 *)
