From Stdlib Require Import Lists.List Arith.Arith.
Import ListNotations.

Inductive ty : Type :=
| TNat : ty.

Inductive expr : Type :=
| EVar : nat -> expr
| EZero : expr
| ESucc : expr -> expr.

Inductive value : expr -> Prop :=
| VZero : value EZero
| VSucc : forall e, value e -> value (ESucc e).

Inductive lookup : list ty -> nat -> ty -> Prop :=
| LHere : forall gamma type,
    lookup (type :: gamma) 0 type
| LThere : forall gamma index type other,
    lookup gamma index type ->
    lookup (other :: gamma) (S index) type.

Inductive has_type : list ty -> expr -> ty -> Prop :=
| T_Var : forall gamma index type,
    lookup gamma index type ->
    has_type gamma (EVar index) type
| T_Zero : forall gamma,
    has_type gamma EZero TNat
| T_Succ : forall gamma e,
    has_type gamma e TNat ->
    has_type gamma (ESucc e) TNat.

Inductive step : expr -> expr -> Prop :=
| S_Succ : forall e e',
    step e e' ->
    step (ESucc e) (ESucc e').

Fixpoint substitute (sigma : nat -> expr) (e : expr) : expr :=
  match e with
  | EVar index => sigma index
  | EZero => EZero
  | ESucc inner => ESucc (substitute sigma inner)
  end.

Definition typed_substitution
    (gamma delta : list ty) (sigma : nat -> expr) : Prop :=
  forall index type,
    lookup gamma index type ->
    has_type delta (sigma index) type.

Theorem substitution_preserves_typing :
  forall gamma delta sigma e type,
    typed_substitution gamma delta sigma ->
    has_type gamma e type ->
    has_type delta (substitute sigma e) type.
Proof.
  intros gamma delta sigma e type Hsigma Htyped.
  revert delta sigma Hsigma.
  induction Htyped; intros delta sigma Hsigma; simpl.
  - apply Hsigma. assumption.
  - constructor.
  - constructor. apply IHHtyped. exact Hsigma.
Qed.

Theorem context_compatibility :
  forall e e',
    step e e' ->
    step (ESucc e) (ESucc e').
Proof.
  intros e e' Hstep. constructor. assumption.
Qed.

Theorem preservation :
  forall gamma e e' type,
    has_type gamma e type ->
    step e e' ->
    has_type gamma e' type.
Proof.
  intros gamma e e' type Htyped Hstep.
  induction Hstep.
  inversion Htyped; subst.
  constructor. apply IHHstep. assumption.
Qed.

Theorem progress :
  forall e,
    has_type [] e TNat ->
    value e \/ exists e', step e e'.
Proof.
  intros e.
  induction e as [index | | inner IH]; intros Htyped.
  - inversion Htyped as [gamma index' type Hlookup | |]; subst.
    inversion Hlookup.
  - left. constructor.
  - inversion Htyped; subst.
    match goal with
    | Hinner : has_type [] inner TNat |- _ =>
        destruct (IH Hinner) as [Hvalue | [next Hstep]]
    end.
    + left. constructor. assumption.
    + right. exists (ESucc next). constructor. assumption.
Qed.

Theorem component_interaction :
  forall e e',
    has_type [] e TNat ->
    step e e' ->
    has_type [] e' TNat.
Proof.
  intros e e' Htyped Hstep.
  eapply preservation; eauto.
Qed.

Theorem bounded_core_composition :
  forall e,
    has_type [] e TNat ->
    value e \/ exists e', step e e' /\ has_type [] e' TNat.
Proof.
  intros e Htyped.
  destruct (progress e Htyped) as [Hvalue | [next Hstep]].
  - left. exact Hvalue.
  - right. exists next. split.
    + exact Hstep.
    + eapply component_interaction; eauto.
Qed.
