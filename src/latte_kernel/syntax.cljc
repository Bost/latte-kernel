
(ns latte-kernel.syntax
  "The internal representation of lambda-terms."
  (:require [clojure.set :as set]))


;;{
;; # The syntax of type theory
;;
;; In this namespace we encode the internal representation of
;; the lambda-terms.
;;}

;;{
;; ## Lambda terms
;;
;; The lambda-terms of LaTTe are composed of:
;;}

;;{
;;   - the sorts *kind* `□` and *type* `✳`
;;}
(defn kind?
  "Is `t` a kind?"
  [t]
  (= t '□))

(defn type?
  "Is `t` a type?"
  [t]
  (= t '✳))

(defn sort?
  "Is `t` a sort?"
  [t]
  (or (kind? t)
      (type? t)))

;;{
;;   - occurrences of *variables*
;;}
(defn variable?
  "Is `t` a variable occurrence?"
  [t]
  (symbol? t))

;;{
;;   - *lambda abstractions* `(λ [x t] u)`
;;   - *product abstractions* `(Π [x t] u)`
;;}
(defn binder?
  "Is `t` a binding abstraction?"
  [t]
  (and (list? t)
       (contains? '#{λ Π} (first t))))

(defn lambda?
  "Is `t` a lambda-abstraction?"
  [t]
  (and (seq? t)
       (= (first t) 'λ)))

(defn prod?
  "Is `t` a product abstraction?"
  [t]
  (and (seq? t)
       (= (first t) 'Π)))

;;{
;;  - *applications* `[u v]`
;;}
(defn app?
  "Is `t` an application?"
  [t]
  (and (vector? t)
       (= (count t) 2)))

;;{
;;  - *references* to named definitions `(f e1 e2 ... eN)`
;;}
(defn ref?
  "Is `t` a reference?"
  [t]
  (and (seq? t)
       (not (contains? '#{λ Π ::ascribe} (first t)))))

;;{
;;  - *ascriptions* `(::ascribe e t)` that term `e` is of type `t`. These are like
;; explicit type assertions that are used internally and cannot
;; result from parsing a term in the user-level syntax.

;; As a safety measure we use the word *ascription* that a user will unlikely
;; use its own code. Moreover, the keyword is namespaces so these should
;; be enought to emphasis the fact that ascriptions are *dangerous* and
;; only used inside the kernel...
;;}

(defn ascription?
  "Is `t` a type ascription?"
  [t]
  (and (seq? t)
       (= (count t) 3)
       (= (first t) ::ascribe)))

;;{
;; Note that type ascriptions do not add any expressivity, but
;; they allow interesting optimizations (ascribed types do not need
;; to be recomputed).
;;}

;;{
;; ... and that's everything you need to capture the
;; essence of mathematics!
;;}

;;{
;; ## Free and bound variables
;;
;; In `(lambda [x t] [x y])` there is one *bound occurrence* of
;; variable `x` and one *free occurrence* of variable `y`.
;;}

(defn free-vars
  "Get the set of free variables of term `t`."
  [t]
  (cond
    (variable? t) #{t}
    (binder? t) (let [[_ [x ty] body] t]
                  (set/union (free-vars ty)
                             (disj (free-vars body) x)))
    (app? t) (set/union (free-vars (first t))
                        (free-vars (second t)))
    (ascription? t) (let [[_ e u] t]
                  (set/union (free-vars e)
                             (free-vars u)))
    (ref? t) (apply set/union (map free-vars (rest t)))
    :else #{}))

(defn vars
  "Get the set of free and bound variables of term `t`."
  [t]
  (cond
    (variable? t) #{t}
    (binder? t) (let [[_ [x ty] body] t]
                  (set/union (vars ty) (vars body)))
    (app? t) (set/union (vars (first t))
                        (vars (second t)))
    (ascription? t) (let [[_ e u] t]
                  (set/union (vars e) (vars u)))
    (ref? t) (apply set/union (map vars (rest t)))
    :else #{}))

(defn bound-vars
  "Get the set of bound variables of term `t`."
  [t]
  (set/difference (vars t) (free-vars t)))

;;{
;; ## Substitution
;;
;; The substitution of free occurrences of variables by
;; terms might be thought of as a simple thing but it is *not*.
;; The algorithm is rather tricky because one has to avoid
;; nameclash. Instead of a locally-nameless solution, which yields
;; a much simpler algorithm we rely on an explicit renaming scheme,
;; quite similar to what can be found in e.g. HOL light.
;;
;; The advantage of explicit renaming is that it is much more
;; robust in further algorithms (especially type inference which
;; is very cumbersome with nameless representations).
;;}

(defn mk-fresh
  "Generate a fresh variable name, with prepfix `base`
  and suffix chosen from ' (quote), '', ''' then -4, -5, etc.
The `forbid` argument says what names are forbidden."
  ([base forbid] (mk-fresh base 0 forbid))
  ([base level forbid]
   (let [suffix (case level
                  0 ""
                  1 "'"
                  2 "''"
                  3 "'''"
                  (str "-" level))
         candidate (symbol (str base suffix))]
     (if (contains? forbid candidate)
       (recur base (inc level) forbid)
       candidate))))

;;{
;; The following is the core of the substitution algorithm.
;;}

(defn- subst-
  "Applies substituion `sub` on term `t`. 
Names generated fresh along the substitution cannot be members of `forbid`.
  Bound variable may be renamed according to the `rebind` map."
  [t sub forbid rebind]
  (let [[t' forbid']
        (cond
          ;; variables
          (variable? t) (if-let [x (get rebind t)]
                          [x (conj forbid t)]
                          [(get sub t t) (conj forbid t)])
          ;; binders (λ, Π)
          (binder? t)
          (let [[binder [x ty] body] t
                [x' rebind' forbid']
                (if (contains? forbid x)
                  (let [x' (mk-fresh x forbid)]
                    [x' (assoc rebind x x') (conj forbid x')])
                  [x rebind (conj forbid x)])
                [ty' forbid''] (subst- ty sub forbid' rebind)
                [body' forbid'''] (subst- body sub forbid'' rebind')]
            ;; (println "term=" (list binder [x' ty'] body') "sub=" sub')
            [(list binder [x' ty'] body')
             forbid'''])
          ;; applications
          (app? t)
          (let [[rator forbid'] (subst- (first t) sub forbid rebind)
                [rand forbid''] (subst- (second t) sub forbid' rebind)]
            [[rator rand] forbid''])
          ;; ascriptions
          (ascription? t)
          (let [[_ e u] t
                [e' forbid'] (subst- e sub forbid rebind)
                [u' forbid''] (subst- u sub forbid' rebind)]
            [(list ::ascribe e' u') forbid''])
          ;; references
          (ref? t) (let [[args forbid']
                         (reduce (fn [[ts forbid] t]
                                   (let [[t' forbid'] (subst- t sub forbid rebind)]
                                     [(conj ts t') forbid'])) ['() forbid] (rest t))]
                     [(conj (into '() args) (first t)) forbid'])
          ;; other cases
          :else [t forbid])]
    ;;(println "[subst-] t=" t "sub=" sub "forbid=" forbid "rebind=" rebind)
    ;;(println "   ==> " t')
    [t' forbid']))

(defn subst
  "Applies substitution `sub` (defaulting to `{x u}`) to term `t`."
  ([t x u] (subst t {x u}))
  ([t sub]
   (let [forbid (set/union
                 (apply set/union (map vars (vals sub)))
                 (into #{} (keys sub))
                 (free-vars t))
         [t' _] (subst- t sub forbid {})]
     t')))

;;{
;; ## Alpha-equivalence
;;
;; Comparing lambda-terms should be up to *alpha-equivalence*.
;; To recover a structural equality, a nameless representation
;; of lambda-terms can be used. Each bound variable is renamed
;; in a canonical way using so-called *Debruijn indices*.

;; The normalization process is described below.
;;}

(defn- alpha-norm- [t sub level]
  (cond
    ;; variables
    (variable? t) [(get sub t t) level]
    ;; binders (λ, Π)
    (binder? t)
    (let [[binder [x ty] body] t
          x' (symbol (str "_" level))
          [ty' level'] (alpha-norm- ty sub (inc level))
          [body' level''] (alpha-norm- body (assoc sub x x') level')]
      [(list binder [x' ty'] body')
       level''])
    ;; applications
    (app? t)
    (let [[left' level'] (alpha-norm- (first t) sub level)
          [right' level''] (alpha-norm- (second t) sub level')]
      [[left' right'] level''])
    ;; ascription
    (ascription? t)
    (let [[_ e u] t
          [e' level'] (alpha-norm- e sub level)
          [u' level''] (alpha-norm- u sub level')]
      [(list ::ascribe e' u') level''])
    ;; references
    (ref? t) (let [[args level']
                   (reduce (fn [[args level] arg]
                             (let [[arg' level'] (alpha-norm- arg sub level)]
                               [(conj args arg') level'])) ['() level] (rest t))]
               [(conj (into '() args) (first t)) level'])
    ;; other cases
    :else [t level]))

(defn alpha-norm
  "Produce a canonical nameless reprensentation of the lambda-term `t`"
  [t]
  (let [[t' _] (alpha-norm- t {} 1)]
    t'))

;;{
;; Now alpha-equivalence of lambda-terms simply
;; boils down to the equality of their nameless representation.
;;}

(defn alpha-eq? [t1 t2]
  (= (alpha-norm t1)
     (alpha-norm t2)))


