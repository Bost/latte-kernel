
(ns latte-kernel.syntax-test
  (:require #?(:clj [clojure.test :refer :all]
               :cljs [cljs.test :as t :refer-macros [is deftest testing]])
            [latte-kernel.syntax :refer :all]))

(defn identify-all
  "Test agains every single parameter predicate from the latte-kernel.syntax (except sort?)"
  [input]
  (->> [kind? type? variable? binder? app? ascription? ref? lambda? prod?]
       (map #(if (% input) %))
       (remove nil?)))

(deftest test-identify-all
  (is (= (identify-all 't)
         [variable?]))

  ;; make sure your editor does not prettify symbols. In emacs see `M-x prettify-symbols-mode`

  ;; testing some of:
  ;; - java reserved words
  ;; - clojure symbols
  ;; - utf8 chars and strings
  ;; - etc.
  (is (= (identify-all 'fn)
         (identify-all 'vector)
         (identify-all 'vec)
         (identify-all 'list)
         (identify-all 'defmacro)
         (identify-all 'type)
         (identify-all '*ns*)
         (identify-all '->)
         (identify-all '->>)
         (identify-all '=)
         (identify-all '=>)
         (identify-all 'get))
         (identify-all 'equals)
         (identify-all 'compare)
         (identify-all 'Boolean)
         (identify-all '.)
         (identify-all 'java.lang.Byte)
         (identify-all 'α)
         (identify-all 'β)
         (identify-all 'γ)
         (identify-all 'ß)
         (identify-all 'Ů)
         (identify-all 'Ůää)
         (identify-all 'αβγ)
         (identify-all '⇧) ;; TODO Does the `⇧` have any special meaning?
         (identify-all 'or)
         (identify-all 'and)
         (identify-all 'not)
         [variable?]))

  ;; `true` and `false` don't belong to any category
  ;; TODO: should we have an own predicate for the category of logical constanst?
  (is (= (identify-all 'true)
         (identify-all 'false)
         []))

  ;; `nil` doesn't belong to any category
  ;; TODO: should we have an own predicate for nil? I.e. some sort of an empty category?
  (is (= (identify-all 'nil)
         []))

  ;; numbers do not belong to any category; TODO: is this correct?
  ;; => (= (type '42) (type 42) java.lang.Long)
  ;; => true
  (is (= (identify-all 4)
         (identify-all 42)
         (identify-all '42)
         []))

  ;; strings and chars do not belong to any category; TODO: is this correct?
  ;; => (type '"")
  ;; => java.lang.String
  ;; => (type '\')
  ;; => java.lang.Character
  (is (= (identify-all '"")
         (identify-all '"foo")
         (identify-all '\\)
         (identify-all '\x)
         (identify-all '\')
         (identify-all '\))
         []))

  (is (= (identify-all '[])
         (identify-all '[x])
         ;; (identify-all '[x y])  ;; this is an application
         (identify-all '[x y z])
         (identify-all '[x y z u])
         (identify-all '{})
         ;; (identify-all '{x}) ;; syntax error - can't build a pair
         (identify-all '{x y})
         ;; (identify-all '{x y z}) ;; syntax error - can't build a pair
         (identify-all '#{})
         (identify-all '#{x})
         (identify-all '#{x y})
         (identify-all '#{x y z})
         (identify-all '#{x y z u})
         []))

  (is (= (identify-all '[x y])
         [app?]))

  (is (= (identify-all '())
         (identify-all '(x))
         (identify-all '(x y))
         (identify-all '(x y z))
         (identify-all '(x y z u))
         [ref?]))

  ;; TODO is this correct?
  (is (= (identify-all 'λ)
         (identify-all 'Π)
         (identify-all '==>)
         (identify-all 'forall)
         (identify-all 'exists)
         (identify-all 'lambda)
         (identify-all '⟶)
         (identify-all '∀)
         (identify-all '∃)
         [variable?]))

  (is (= (identify-all '(λ [x t] x))
         [binder? lambda?]))

  (is (= (identify-all '(λ [x t]))
         [binder? lambda?]))

  (is (= (identify-all '(Π [x t] x))
         [binder? prod?]))

  (is (= (identify-all '(Π [x t]))
         [binder? prod?]))

  ;; `✳` is a type and a variable at the same time; TODO: is this correct?
  (is (= (identify-all '✳)
         [type? variable?]))

  ;; `□` is a kind and a variable at the same time; TODO: is this correct?
  (is (= (identify-all '□)
         [kind? variable?])))

(deftest test-term-reduce
  (is (= (term-reduce {} 42 '(λ [x t] (test x y z)))
         42))

  (is (= (term-reduce {:var (fn [vs v] (conj vs v))} #{} '(λ [x t] (test x y z)))
         '#{x y z t})))

(deftest test-free-vars
  (is (= (free-vars 'x)
         #{'x}))
  
  (is (= (free-vars '[x y])
         #{'x 'y}))
  
  (is (= (free-vars '(λ [x t] [x [y z]]))
         #{'t 'y 'z}))
  
  (is (= (free-vars '(Π [x t] [x [y z]]))
         #{'t 'y 'z}))
  
  (is (= (free-vars '(λ [x t] (test x y z)))
         '#{t y z}))

  (is (= (free-vars '(:latte-kernel.syntax/ascribe x [y z]))
         '#{x y z}))
  
  (is (= (free-vars '(λ [x y] (:latte-kernel.syntax/ascribe x [y z])))
         '#{y z})))

(deftest test-vars- ;; nameclash
  (is (= (vars 'x)
       #{'x}))
  
  (is (= (vars '[x y])
         #{'x 'y}))
  
  (is (= (vars '(λ [x t] (test x [y z])))
         #{'t 'x 'y 'z}))
  
  (is (= (vars '(Π [x t] (test x [y z])))
         #{'t 'x 'y 'z}))

  (is (= (vars '(:latte-kernel.syntax/ascribe x [y z]))
         '#{x y z}))
  
  (is (= (vars '(λ [x y] (:latte-kernel.syntax/ascribe x [y z])))
         '#{x y z})))

(deftest test-bound-vars
  (is (= (bound-vars 'x)
         #{}))

  (is (= (bound-vars '[x y])
         #{}))

  (is (= (bound-vars '(λ [x t] (test x [y z])))
         #{'x}))

  (is (= (bound-vars '(λ [x t] (test t [y z])))
         #{}))

  (is (= (bound-vars '(Π [x t] (test x [y z])))
         #{'x}))
  
  (is (= (bound-vars '(:latte-kernel.syntax/ascribe x [y z]))
         '#{}))
  
  (is (= (bound-vars '(λ [x y] (:latte-kernel.syntax/ascribe x [y z])))
         '#{x})))

(deftest test-mk-fresh
  (is (= (mk-fresh 'x '#{x x' x''})
         'x'''))

  (is (=(mk-fresh 'x '#{x x' x'' x'''})
        'x-4)))

(deftest test-subst
  (is (= (subst 'x {'x '✳})
         '✳))

  (is (= (subst 'y {'x '✳})
         'y))

  (is (= (subst '[y x] {'x '✳})
         '[y ✳]))

  (is (= (subst '(:latte-kernel.syntax/ascribe x y) '{x ✳, y z})
         '(:latte-kernel.syntax/ascribe ✳ z)))

  (is (= (subst '[x (λ [x ✳] (test x y z y))] {'x '✳, 'y '□})
         '[✳ (λ [x' ✳] (test x' □ z □))]))

  (is (= (subst '[x (Π [x ✳] [y x])] {'x '✳, 'y 'x})
         '[✳ (Π [x' ✳] [x x'])]))

  (is (= (subst '(λ [x ✳] (test x y (λ [x ✳] (test x y z)) y)) {'x ':replace})
         '(λ [x' ✳] (test x' y (λ [x'' ✳] (test x'' y z)) y))))

  (is (= (subst '(λ [x ✳] (test x y (λ [x ✳] (test x y x')) y)) {'x :replace-x
                                                                 'x' :replace-x' })
         '(λ [x'' ✳] (test x'' y (λ [x''' ✳] (test x''' y :replace-x')) y))))

  (is (= (subst '(test x y (λ [x ✳] (test x y x')) y) {'x :replace-x
                                                       'x' :replace-x' })
         '(test :replace-x y (λ [x'' ✳] (test x'' y :replace-x')) y)))

  (is  ;; XXX: this comes from a very subtile bug !
   (= (subst '(Π [⇧ (Π [x' T] (Π [⇧ (Π [x T] (Π [⇧ [X x]] [[R x] x']))] [R z]))]
                 [R z]) 'z 'x)
      '(Π [⇧ (Π [x' T] (Π [⇧' (Π [x'' T] (Π [⇧'' [X x'']] [[R x''] x']))] [R x]))] [R x]))))

(deftest test-alpha-norm
  (is (= (alpha-norm 'x)
         'x))

  (is (= (alpha-norm '(λ [x ✳] x))
         '(λ [_1 ✳] _1)))

  (is (= (alpha-norm '[x (λ [x ✳] (test x y [x z]))])
         '[x (λ [_1 ✳] (test _1 y [_1 z]))])))

(deftest test-alpha-eq?
  (is (= (alpha-eq? '(λ [x ✳] x)
                    '(λ [y ✳] y))
         true))
  ;; the type part of an ascription is not
  ;; considered for alpha-equivalence
  (is (= (alpha-eq? '(:latte-kernel.syntax/ascribe (λ [x ✳] x) (∀ [A □] A))
                    '(λ [z ✳] z)))))


(deftest test-top-assumption
  (is (= (top-assumption '(Π [x T] U)) '[T U]))
  (is (= (top-assumption '(λ [x T] U)) '[nil (λ [x T] U)]))
  )

(deftest test-assumptions
  (is (= (assumptions '(Π [x T] U)) '[T]))
  (is (= (assumptions '(Π [x T] (Π [y U] V))) '[T U]))
  )
