(ns latte-kernel.typing
  (:require [latte-kernel.utils :as u]
            [latte-kernel.syntax :as stx]
            [latte-kernel.norm :as norm]
            [latte-kernel.defenv :as defenv]))
  
;;{
;; # Type checking
;;
;; Type checking, and more precisely **type inference**, is the *heart* of a
;; proof assistant based on type theory.
;;
;; In this namespace all the type-checking rules are implemented by dedicated
;; functions.
;;}

;;{
;; ## Type context
;;
;; We simply use a vector for the type context, which allows to
;; maintain the scoping rules at the price of some inefficiency
;; (fetching is O(n)). 
;;}

(defn ctx-fetch
  "Look for variable named `x` in the type context `ctx`.
  Returns the associated type or `nil` if there is no such variable."
  [ctx x]
  ;; (println "[ctx-fetch] ctx=" ctx "x=" x)
  (if (seq ctx)
    (if (= (first (first ctx)) x)
      (second (first ctx))
      (recur (rest ctx) x))
    nil))

(defn ctx-put
  "Add variable `x` bound to type `t` in context `ctx`, potentially
  shadowing other variables with the same name."
  [ctx x t]
  (cons [x t] ctx))

;;{
;; ## Synthesis rules
;;
;; All the type inference rules are given below.
;;
;;}

(declare type-of-type
         type-of-var
         type-of-prod
         type-of-abs
         type-of-app
         type-of-ref
         type-of-ascribe)

;;{
;; The following function is the main entry point for type inference.
;;}

(defn type-of-term
  "Infer the type of term `t` in definitional environment `def-env` 
  and type context `ctx`.
  The returned value is of the form `[:ok <type>]` if the inferred
  type is `<type>`, or `[:ko <info>]` in case of failure, with `<info>`
 an error map."
  [def-env ctx t]
  (let [[status ty]
        (cond
          (stx/kind? t) [:ko {:msg "Kind has not type" :term t}]
          (stx/type? t) (type-of-type)
          (stx/variable? t) (type-of-var def-env ctx t)
          ;; binders (lambda, prod)
          (stx/binder? t)
          (let [[binder [x ty] body] t]
            (case binder
              λ (type-of-abs def-env ctx x ty body)
              Π (type-of-prod def-env ctx x ty body)
              (throw (ex-info "No such binder (please report)" {:term t :binder binder}))))
          ;; references
          (stx/ref? t) (type-of-ref def-env ctx (first t) (rest t))
          ;; ascriptions
          (stx/ascription? t)
          (let [[_ e u] t]
            (type-of-ascribe def-env ctx e u))
          ;; applications
          (stx/app? t) (type-of-app def-env ctx (first t) (second t))
          :else
          (throw (ex-info "Invalid term (please report)" {:term t})))]
    ;;(println "--------------------")
    ;;(println "[type-of-term] t=" t)
    ;;(clojure.pprint/pprint ty)
    ;;(println "--------------------")
    [status ty]))

;;{
;; Type-checking is a derived form of type inference. Given a term `t`
;; and a type `T`, the first step is to infer the type of `t`, say `U`,
;; and then check that it is beta-equivalent to the expected type `T`.
;;}

(defn type-check?
  "Check if `term` has the given `type` in the definitional
  environment `def-env` and context `ctx`."
  [def-env ctx term type]
  ;;(println "[type-check?] term=" term "type=" type)
  ;;(println "    ctx=" ctx)
  (let [[status type'] (type-of-term def-env ctx term)]
    ;;(println "  ==> " status "type'=" type' "vs. type=" type)
    (if (= status :ok)
      (norm/beta-eq? def-env ctx type type')
      (throw (ex-info "Cannot check type of term" {:term term :from type'})))))

;;{
;;
;; ### The type of types
;;
;; The type of *the type of types* `✳` (or `:type`) is the kind `□` (or `:kind`).
;;
;; **Remark**: LaTTe uses an *impredicative* type theory, marked by the fact that
;; the kind `□` itself has no type.
;;  
;;     --------------------
;;     E |- Type ::> Kind
;;
;;}

(defn type-of-type
  "Return type type of `:type`."
  []
  [:ok '□])

;;{
;;
;;  ### The type of variables
;;
;;  The type of a variable is to be found in the context.
;;  And it must be associated to a type or a kind.
;;
;;
;;        ty::>Type or t::>Kind in E
;;     ------------------------------
;;        E,x::ty |- x ::> ty
;;}

(defn type-of-var
  "Infer the type of variable `x` in context `ctx`."
  [def-env ctx x]
  (if-let [ty (ctx-fetch ctx x)]
    (let [[status sort] (let [ty' (norm/normalize def-env ctx ty)]
                          (if (stx/kind? ty')
                            [:ok ty']
                            (type-of-term def-env ctx ty)))]
      (if (= status :ko)
        [:ko {:msg "Cannot calculate type of variable." :term x :from sort}]
        (if (stx/sort? sort)
          [:ok ty]
          [:ko {:msg "Not a correct type (super-type is not a sort)" :term x :type ty :sort sort}])))
    [:ko {:msg "No such variable in type context" :term x}]))


;;{
;;
;; ### The type of products
;;
;;  The type of a product is a *sort*, either the type of types or the
;; type of kinds.
;;
;;    E |- A ::> s1     E,x:A |- B ::> s2
;;    -----------------------------------
;;     E |- prod x:A . B  ::>  s2
;;}

(defn type-of-prod
  "Infer the type of a product with bound variable `x` of
  type `A` in body `B`."
  [def-env ctx x A B]
  (let [[status sort1] (type-of-term def-env ctx A)]
    (if (= status :ko)
      [:ko {:msg "Cannot calculate domain type of product." :term A :from sort1}]
      (let [sort1' (norm/normalize def-env ctx sort1)]
        (if (not (stx/sort? sort1'))
          [:ko {:msg "Not a valid domain type in product (super-type not a sort)" :term A :type sort1}]
          (let [ctx' (ctx-put ctx x A)
                [status sort2] (type-of-term def-env ctx' B)]
            (if (= status :ko)
              [:ko {:msg "Cannot calculate codomain type of product." :term B :from sort2}]
              (let [sort2' (norm/normalize def-env ctx sort2)]
                ;; (println "sort2' = " sort2' " sort? " (stx/sort? sort2'))
                (if (not (stx/sort? sort2'))
                  [:ko {:msg "Not a valid codomain type in product (not a sort)" :term B :type sort2}]
                  [:ok sort2])))))))))


;;{
;;
;; ### The type of abstractions
;;
;; The type of an abstraction is simply the coresponding
;; product, on of the beauties of type theory.
;;
;;
;;    E,x:A |- t ::> B  E |- prod x:A. B ::> s
;;    --------------------------------------------
;;    E |- lambda x:A . t  ::>  prod x:A . B
;;}


(defn type-of-abs
  "Infer the type of an  with bound variable `x` of
  type `A` in body `B`."
  [def-env ctx x A t]
  (let [ctx' (ctx-put ctx x A)
        [status B] (type-of-term def-env ctx' t)]
    (if (= status :ko)
      [:ko {:msg "Cannot calculate codomain type of abstraction."
            :term (list 'λ [x A] t) :from B}]
      (let [tprod (list 'Π [x A] B)
            [status sort] (type-of-term def-env ctx tprod)]
        (if (= status :ko)
          [:ko {:msg "Not a valid codomain type in abstraction (cannot calculate super-type)."
                :term (list 'λ [x A] t)
                :codomain B :from sort}]
          (if (not (stx/sort? (norm/normalize def-env ctx sort)))
            [:ko {:msg "Not a valid codomain type in abstraction (super-type not a sort)."
                  :term (list 'λ [x A] t)
                  :codomain B
                  :type sort}]
            [:ok tprod]))))))


;;{
;;
;; ### The type of applications
;;
;; The typing of an application is a little bit more demanding,
;; especially because it involves the substitution of the
;; bound variable by the operand in the return type.
;;
;;
;;       E |- rator ::> prod x:A . B    E|- rand :: A
;;    -------------------------------------------------
;;          E |- rator rand : B [rand/x]
;;}

(defn type-of-app
  "Infer the type of an application with operator `rator` and
  operand `rand`."
  [def-env ctx rator rand]
  (let [[status trator] (type-of-term def-env ctx rator)]
    (if (= status :ko)
      [:ko {:msg "Cannot calculate operator (left-hand) type in application."
            :term [rator rand] :from trator}]
      (let [trator' (norm/normalize def-env ctx trator)]
        (if (not (stx/prod? trator'))
          [:ko {:msg "Not a product type for operator (left-hand) in application." :term [rator rand] :operator-type trator}]
          (let [[_ [x A] B] trator']
            ;; (println "[type-of-app] trator'=" trator')
            (if (not (type-check? def-env ctx rand A))
              [:ko {:msg "Cannot apply: type domain mismatch" :term [rator rand] :domain A :operand rand}]
              (do ;;(println "[type-of-app] subst...")
                  ;;(println "    B = " B)
                  ;;(println "    x = " x)
                  ;;(println "    rand = " rand)
                  (let [res (stx/subst B x rand)]
                    ;;(println "   ===> " res)
                    [:ok res])))))))))

;;{
;;
;; ### The type of references
;;
;; A reference to a defined term in LaTTe is like a 'function call' in a
;; programming language. As such, in order to type a reference
;; one has to unfold the reference by the defined term.
;; The type is then inferred from the unfolded term.
;;
;;
;;    D |- ref :: [x1 t1] [x2 t2] ... [xN tN] -> t
;;    E |- e1 :: t1   E, x1:t1 |- e2 :: t2
;;    ...
;;    E, x1:t1, ..., xM-1:tM-1 |- eM :: tM
;; -------------------------------------------------------------------------------------
;;      D, E |- (ref e1 e2 ... eM)
;;              ::> (prod [xM+1 tM+1] ... (prod [xN tN] t [e1/x1, e2/x2, ...eM/xM]) ...)
;;
;;}

;;{
;; A reference is of the form `(ref e1 e2 ... eM)` where `ref` is a name and the `ei`'s are arbitrary expressions.
;; It can be a reference to either:
;;  - a defined term such as a parametric definition or an axiom
;;  - a theorem, which is a particular case of a defined term
;;  - an implicit
;;
;; Both the two first cases are handled by the function `type-of-refdef` below. The only difference
;; is that a theorem is a defined term only if it has been demonstrated. Put in other terms, it
;; is forbidden to reference a theorem with no-proof. The third case allows to perform arbibrary
;; computations during the type synthesis phase, it is handled by the `type-of-implicit` function.
;;}

(declare type-of-refdef)
(declare type-of-implicit)

(defn type-of-ref [def-env ctx name args]
  (let [[status ty]
        (let [[status ddef] (defenv/fetch-definition def-env name)]
          (cond
            (= status :ko) [:ko ddef]
            (not (defenv/latte-definition? ddef))
            (throw (ex-info "Not a LaTTe definition (please report)." {:def ddef}))
            (defenv/special? ddef)
            (throw (ex-info "Special should not occur at typing time (please report)"
                            {:special ddef :term (list* name args)}))
            (defenv/notation? ddef)
            (throw (ex-info "Notation should not occur at typing time (please report)"
                            {:notation ddef :term (list* name args)}))
            (and (defenv/theorem? ddef)
                 (= (:proof ddef) false))
            [:ko {:msg "Theorem has no proof." :thm-name (:name ddef)}]
            (defenv/implicit? ddef)
            (type-of-implicit def-env ctx ddef args)
            (and (not (defenv/definition? ddef))
                 (not (defenv/theorem? ddef)))
            (throw (ex-info "Unsupported definitional entity, expecting a true definition or a theorem name"
                            {:name name, :entity ddef}))
            (> (count args) (:arity ddef))
            [:ko {:msg "Too many arguments for definition." :term (list* name args) :arity (:arity ddef)}]
            :else
            (type-of-refdef def-env ctx name ddef args)))]
    ;;(println "---------------------")
    ;;(println "[type-of-ref] name=" name "args=" args)
    ;;(clojure.pprint/pprint ty)
    ;;(println "---------------------")
    [status ty]))

;;{
;; #### Typing defined terms
;;
;; The standard processing of a reference is to construct the lambda-term corresponding
;; to the unfolding of a defined term. This is by substituting the parameters of the
;; defined term by the arguments of the reference. LaTTe allows the partial unfolding
;; of the defined terms, thus at the end we generalise for the remaining
;; uninstantiated parameters (as lambda-abstractions).
;;}

(declare prepare-argument-subst)
(declare generalize-params)

(defn type-of-refdef [def-env ctx name ddef args]
  (let [[status, res] (prepare-argument-subst def-env ctx name args (:params ddef))]
    (if (= status :ko)
      [:ko res]
      (let [[params sub] res
            expanded-term (stx/subst (:type ddef) sub)
            typ (generalize-params (reverse params) expanded-term)]
        ;;(println "[type-of-refdef] typ = " typ)
        [:ok typ]))))

;;{
;; The function below realizes the substitution of the parameters by
;; their corresponding argument. The substitution `sub` is represented
;; as a map.
;;}

(defn prepare-argument-subst [def-env ctx name args params]
  (loop [args args, params params, sub {}]
    ;; (println "args=" args "params=" params "sub=" sub)
    (if (seq args)
      (let [arg (first args)
            ty (stx/subst (second (first params)) sub)]
        ;; (println "arg=" arg "ty=" ty)
        (if (not (type-check? def-env ctx arg ty))
          [:ko {:msg "Wrong argument type"
                :term (list* name args)
                :arg arg
                :expected-type ty}]
          (recur (rest args) (rest params) (assoc sub (ffirst params) arg))))
      ;; all args have been checked
      [:ok [params sub]])))

;;{
;; The following function generalizes the remaining uninstantiated parameters.
;;}

(defn generalize-params [params res-type]
  (loop [params params, res res-type]
    (if (seq params)
      (recur (rest params) (list 'Π (first params) res))
      res-type)))

(declare type-of-args)

(defn type-of-implicit [def-env ctx implicit-def args]
  (let [[status, targs] (type-of-args def-env ctx args)]
    (if (= status :ko)
      targs
      (let [[status, implicit-term]
            (try (apply (:implicit-fn implicit-def) def-env ctx targs)
                 (catch Exception exc
                   [:ko (merge {:implicit (:name implicit-def)
                                :msg (.getMessage exc)}
                               (ex-data exc))]))]
        (if (= status :ko)
          implicit-term
          ;; recursive typing of implicit-generated term
          (type-of-term def-env ctx implicit-term))))))

(defn type-of-args [def-env ctx args]
  (loop [args args, targs []]
    (if (seq args)
      (let [[status typ] (type-of-term def-env ctx (first args))]
        (if (= status :ko)
          typ
          (recur (rest args) (conj targs [(first args) typ]))))
      [:ok targs])))

(comment


(defn type-of
  ([t] (type-of {} [] t))
  ([ctx t] (type-of {} ctx t))
  ([def-env ctx t]
   (let [[status ty] (type-of-term def-env ctx t)]
     (if (= status :ko)
       (throw (ex-info "Type checking error" ty))
       ty))))

(defn proper-type?
  ([t] (proper-type? {} [] t))
  ([ctx t] (proper-type? {} ctx t))
  ([def-env ctx t]
   (let [ty (type-of def-env ctx t)]
     (let [sort (norm/normalize def-env ctx ty)]
       (stx/sort? sort)))))

)