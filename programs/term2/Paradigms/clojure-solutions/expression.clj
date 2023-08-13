(defn constant [val] (fn [_] val))
(defn variable [val] (fn [keys] (keys val)))

(defn function [f]
  (fn [& args]
    (fn [keys]
      (let [res (map (fn [expr] (expr keys)) args)] (apply f res)))))


(def add (function +))
(def subtract (function -))
(def multiply (function *))
(def negate (function -))
(def divide (function (fn [num & dens] (let [den (apply * dens)] (if (= (double den) 0.0) ##Inf (/ num den))))))
(def arcTan (function (fn [arg] (Math/atan arg))))
(def arcTan2 (function (fn [arg1 arg2] (Math/atan2 arg1 arg2))))




(defn evaluate [expr keys] ((expr :evaluate) expr keys))
(defn toString [expr] ((expr :toString) expr))

(defn Constant [val]
  {:val      val
   :evaluate (fn [expr _] (expr :val))
   :toString (fn [expr] (format "%.1f" (double (expr :val))))})

(defn Variable [val]
  {:val val
   :evaluate (fn [expr keys] (keys (expr :val)))
   :toString (fn [expr] (expr :val))})

(defn UnaryOperation [f sign]
  (fn [val]
    {:val val
     :f f
     :sign sign
     :evaluate (fn [expr keys] ((expr :f) (evaluate (expr :val) keys)))
     :toString (fn [expr] (str "(" (expr :sign) " " (toString (expr :val)) ")"))}))

(defn Operation [f sign]
  (fn [left right]
    {:left left
     :right right
     :f f
     :sign sign
     :evaluate (fn [expr keys] ((expr :f) (evaluate (expr :left) keys) (evaluate (expr :right) keys)))
     :toString (fn [expr] (str "(" (expr :sign) " " (toString (expr :left)) " " (toString (expr :right)) ")"))}))

(def Negate (UnaryOperation - "negate"))
(def Add (Operation + "+"))
(def Subtract (Operation - "-"))
(def Multiply (Operation * "*"))
(def Divide (Operation (fn [num & dens] (let [den (apply * dens)] (if (= (double den) 0.0) ##Inf (/ num den)))) "/"))
(def Sinh (UnaryOperation (fn [arg] (Math/sinh arg)) "sinh"))
(def Cosh (UnaryOperation (fn [arg] (Math/cosh arg)) "cosh"))



(def operations {'+ add
                 '- subtract
                 '* multiply
                 '/ divide
                 'negate negate
                 'atan arcTan
                 'atan2 arcTan2})

(def Operations {'+ Add
                 '- Subtract
                 '* Multiply
                 '/ Divide
                 'negate Negate
                 'sinh Sinh
                 'cosh Cosh})

(defn parse [opMap type]
  (fn [expression]
  (cond
    (seq? expression) (apply (opMap (first expression)) (mapv (parse opMap type) (rest expression) ))
    (number? expression) (if (= type "Func") (constant expression) (if (= type "Obj") (Constant expression)))
    (symbol? expression) (if (= type "Func") (variable (str expression)) (if (= type "Obj") (Variable (str expression))) ))))

(defn parseFunction [string] ((parse operations "Func") (read-string string) ))
(defn parseObject [string] ((parse Operations "Obj") (read-string string)))


