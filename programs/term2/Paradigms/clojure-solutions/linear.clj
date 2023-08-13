(defn v [f] (fn [& vectors] (apply mapv f vectors)))

(def v+ (v +))
(def v- (v -))
(def v* (v *))
(def vd (v /))

(defn scalar [& vectors] (apply + (apply v* vectors)))

(defn vect [vector1 vector2] (vector
                               (- (* (vector1 1) (vector2 2)) (* (vector1 2) (vector2 1)))
                               (- (* (vector1 2) (vector2 0)) (* (vector1 0) (vector2 2)))
                               (- (* (vector1 0) (vector2 1)) (* (vector1 1) (vector2 0)))
                               ))

(defn v*s [vector & scalars] (mapv (partial * (apply * scalars)) vector ))


(defn transpose [matrix] (apply mapv vector matrix))

(defn m [f] (fn [& matrices] (apply mapv (v f) matrices)))

(def m+ (m +))
(def m- (m -))
(def m* (m *))
(def md (m /))

(defn m*s [matrix & scalars] (mapv (fn [vector] (v*s vector (apply * scalars))) matrix))

(defn m*v [matrix vector] (mapv (fn [new_vector] (scalar new_vector vector)) matrix))

(defn m*m [matrixL matrixR] (transpose (mapv (fn [vector] (m*v matrixL vector)) (transpose matrixR))))


(defn s [f] (fn [& args] (apply (fn [arg1 arg2] (if (vector? arg1) (mapv (s f) arg1 arg2) (f arg1 arg2))) args)))

(def s+ (s +))
(def s- (s -))
(def s* (s *))
(def sd (s /))