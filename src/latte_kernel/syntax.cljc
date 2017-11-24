
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
;; - *let abstraction* `(let [x A t] u)`. This can be useful
;; in the concrete syntax (although not as in a programming case)
;; but it is more important to avoid substitutions in typing.
;;}

(defn let?
  "Is `t` a let abstraction?"
  [t]
  (and (seq? t)
       (= (first t) 'let)))

(defn letify
  "Generate a let-abstraction from a sequence of `bindings` and a `body`."
  [bindings body]
  (loop [bindings (reverse bindings), res body]
    (if (seq bindings)
      (recur (rest bindings) (list 'let (first bindings) res))
      res)))


;;{
;;  - *applications* `[u v]`
;;}
(defn app?
  "Is `t` an application?"
  [t]
  (and (vector? t)
       (= (count t) 2)))

;;{
;;  - *references* to named definitions `(f {X1 t1, ..., Xn tn} e1 e2 ... eN)`
;;}
(defn ref?
  "Is `t` a reference?"
  [t]
  (and (seq? t)
       (not (contains? '#{λ Π let ::ascribe} (first t)))))

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
       (= (first t) ::ascribe)))

;;{
;; Note that type ascriptions do not add any expressivity, but
;; they allow interesting optimizations (ascribed types do not need
;; to be recomputed).
;;}

;;{
;; ... and that's every (and even a little bit too much) things
;; we need to capture the essence of mathematics!
;;}

;;{
;; ## Term reducer
;;
;; A reducer for terms.
;;
;;}

(defn term-reduce [red-funs init t]
  (cond
    (kind? t) (if-let [kind-fn (get red-funs :kind)]
                (kind-fn init)
                init)
    (type? t) (if-let [type-fn (get red-funs :type)]
                (type-fn init)
                init)
    (variable? t) (if-let [var-fn (get red-funs :var)]
                    (var-fn init t)
                    init)
    (binder? t) (let [[_ [x ty] body] t
                      bind-kind (if (lambda? t) :lambda :prod)
                      ty-val (term-reduce red-funs init ty)
                      body-val (term-reduce red-funs ty-val body)]
                  (if-let [binder-fn (get red-funs bind-kind)]
                    (binder-fn body-val x)
                    body-val))
    (let? t) (let [[_ [x ty xval] body] t
                   ty-red (term-reduce red-funs init ty)
                   xval-red (term-reduce red-funs ty-red xval)
                   body-red (term-reduce red-funs xval-red body)]
               (if-let [let-fn (get red-funs :let)]
                 (let-fn body-red x)
                 body-red))
    (app? t) (let [[t1 t2] t
                   val1 (term-reduce red-funs init t1)
                   val2 (term-reduce red-funs val1 t2)]
               (if-let [app-fn (get red-funs :app)]
                 (app-fn val2)
                 val2))
    (ascription? t) (let [[t1 t2] t
                          val1 (term-reduce red-funs init t1)
                          val2 (term-reduce red-funs val1 t2)]
                      (if-let [asc-fn (get red-funs :ascribe)]
                        (asc-fn val2)
                        val2))
    (ref? t) (let [[dname & args] t
                   args-val (reduce (fn [val arg]
                                      (term-reduce red-funs val arg)) init args)]
               (if-let [ref-fn (get red-funs :ref)]
                 (ref-fn args-val dname)
                 args-val))
    :else (throw (ex-info "Cannot term reduce: unknown (sub-)term" {:term t}))))

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
    (let? t) (let [[_ [x ty xval] body] t]
               (set/union (free-vars ty)
                          (free-vars xval)
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
    (let? t) (let [[_ [x ty xval] body] t]
               (set/union (vars ty) (vars xval) (vars body)))
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

(declare rebinder)

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
                [x' rebind' forbid'] (rebinder x rebind forbid)
                [ty' forbid''] (subst- ty sub forbid' rebind)
                [body' forbid'''] (subst- body sub forbid'' rebind')]
            [(list binder [x' ty'] body')
             forbid'''])
          ;; let abstractions
          (let? t)
          (let [[_ [x ty xval] body] t
                [x' rebind' forbid'] (rebinder x rebind forbid)
                [ty' forbid''] (subst- ty sub forbid' rebind)
                [xval' forbid'''] (subst- xval sub forbid'' rebind)
                [body' forbid''''] (subst- body sub forbid''' rebind')]
            [(list 'let [x' ty' xval'] body')
             forbid''''])
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

(defn rebinder [x rebind forbid]
  "Rebind `x` if it is present in `forbid`."
  (if (contains? forbid x)
    (let [x' (mk-fresh x forbid)]
      [x' (assoc rebind x x') (conj forbid x')])
    [x rebind (conj forbid x)]))

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

(defn renaming
  "Applies a simple renaming or variables. An assumption is that the
  renaming variables are fresh."
  [t ren]
  (if (empty? ren)
    t
    (cond
      ;; variables
      (variable? t) (get ren t t)
      ;; binders
      (binder? t)
      (let [[binder [x ty] body] t
            ty' (renaming ty ren)
            body' (renaming body (dissoc ren x))]
        (list binder [x ty'] body'))
      ;; let abstractions
      (let? t)
      (let [[_ [x ty xval] body] t
            ty' (renaming ty ren)
            xval' (renaming xval ren)
            body' (renaming body (dissoc ren x))]
        (list 'let [x ty' xval'] body'))
      ;; applications
      (app? t)
      (let [[left right] t
            left' (renaming left ren)
            right' (renaming right ren)]
        [left' right'])
      ;; ascriptions
      (ascription? t)
      (let [[_ term type] t
            term' (renaming term ren)
            type' (renaming type ren)]
        (list ::ascribe term' type'))
      ;; references
      (ref? t)
      (let [args (reduce (fn [args' arg]
                           (conj args' (renaming arg ren))) [] (rest t))]
        (cons (first t) (seq args)))
      :else
      t)))

(declare fresh-binder)

(defn noclash
  "Rewrites a term such that there is no clash for bound variables."
  ([t] (noclash #{} {} t))
  ([forbid t] (noclash forbid) {} t)
  ([forbid ren t]
   (cond
     ;; variables
     (variable? t) (get ren t t)
     ;; binders
     (binder? t)
     (let [[binder [x ty] body] t
           [x' forbid' ren'] (fresh-binder forbid ren x)
           ty' (noclash forbid ren ty)
           body' (noclash forbid' ren' body)]
       (list binder [x' ty'] body'))
     ;; let abstraction
     (let? t)
     (let [[_ [x ty xval] body] t
           [x' forbid' ren'] (fresh-binder forbid ren x)
           ty' (noclash forbid ren ty)
           xval' (noclash forbid ren xval)
           body' (noclash forbid' ren' body)]
       (list 'let [x' ty' xval'] body'))
     ;; applications
     (app? t)
     (let [[left right] t
           left' (noclash forbid ren left)
           right' (noclash forbid ren right)]
       [left' right'])
     ;; ascriptions
     (ascription? t)
     (let [[_ term type] t
           term' (noclash forbid ren term)
           type' (noclash forbid ren type)]
       (list ::ascribe term' type'))
     ;; references
     (ref? t)
     (let [args (reduce (fn [args' arg]
                          (conj args' (noclash forbid ren arg)) [] (rest t)))]
       (cons (first t) (seq args)))
     :else
     t)))

(defn fresh-binder [forbid ren x]
  "Generate a fresh binder name."
  (if (forbid x)
    (let [x' (mk-fresh x forbid)]
      [x' (conj forbid x') (assoc ren x x')])
    [x (conj forbid x) ren]))

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
    ;; let abstraction
    (let? t)
    (let [[_ [x ty xval] body] t
          x' (symbol (str "_" level))
          [ty' level'] (alpha-norm- ty sub (inc level))
          [xval' level''] (alpha-norm- xval sub level')
          [body' level'''] (alpha-norm- body (assoc sub x x') level'')]
      [(list 'let [x' ty' xval'] body')
       level'''])
    ;; applications
    (app? t)
    (let [[left' level'] (alpha-norm- (first t) sub level)
          [right' level''] (alpha-norm- (second t) sub level')]
      [[left' right'] level''])
    ;; ascription
    (ascription? t)
    (let [[_ e _] t
          [e' level'] (alpha-norm- e sub level)
          ;; XXX: the type part is removed during alpha-normalization, it's what's needed ?
          ]
      [e' level'])
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



