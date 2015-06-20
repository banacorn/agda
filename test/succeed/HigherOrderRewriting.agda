open import Common.Prelude
open import Common.Equality

{-# BUILTIN REWRITE _≡_ #-}

postulate is-id : ∀ {A : Set} → (A → A) → Bool
postulate is-id-true : ∀ {A} → is-id {A} (λ x → x) ≡ true

{-# REWRITE is-id-true #-}

test₁ : is-id {Nat} (λ x → x) ≡ true
test₁ = refl

postulate is-const : ∀ {A : Set} → (A → A) → Bool
postulate is-const-true : ∀ {A} (x : A) → is-const (λ _ → x) ≡ true

{-# REWRITE is-const-true #-}

test₂ : is-const {Nat} (λ _ → 42) ≡ true
test₂ = refl

ap : {A B : Set} (f : A → B) {x y : A} →
  x ≡ y → f x ≡ f y
ap f refl = refl

record _×_ (A B : Set) : Set where
  constructor _,_
  field
    fst : A
    snd : B
open _×_

{- Cartesian product -}

module _ {A B : Set} where

  _,=_ : {a a' : A} (p : a ≡ a') {b b' : B} (q : b ≡ b') → (a , b) ≡ (a' , b')
  refl ,= refl = refl

  ap-,-l : (b : B) {a a' : A} (p : a ≡ a') →
    ap (λ z → z , b) p ≡ (p ,= refl)
  ap-,-l b refl = refl
  {-# REWRITE ap-,-l #-}

  test₃ : (b : B) {a a' : A} (p : a ≡ a') →
    ap (λ z → z , b) p ≡ (p ,= refl)
  test₃ _ _ = refl

  ap-,-r : (a : A) {b b' : B} (p : b ≡ b') →
    ap (λ z → a , z) p ≡ (refl ,= p)
  ap-,-r a refl = refl
  {-# REWRITE ap-,-r #-}

  test₄ : (a : A) {b b' : B} (p : b ≡ b') →
    ap (λ z → a , z) p ≡ (refl ,= p)
  test₄ a _ = refl

{- Dependent paths -}

module _ where
  PathOver : {A : Set} (B : A → Set)
    {x y : A} (p : x ≡ y) (u : B x) (v : B y) → Set
  PathOver B refl u v = (u ≡ v)

  postulate
    PathOver-triv : {A B : Set} {x y : A} (p : x ≡ y) (u v : B) →
      (PathOver (λ _ → B) p u v) ≡ (u ≡ v)
  {-# REWRITE PathOver-triv #-}

  test₅ : (p : 1 ≡ 1) → PathOver (λ _ → Nat) p 2 2
  test₅ p = refl
