(ns advent-of-code.day-23
  (:require [clojure.test :refer :all]
            [clojure.string :as str]
            [advent-of-code.day-18
             :refer [command inc-pc reg-or-val read-instructions start-state]]))

(defmethod command 'sub [state [_ a b]]
  (inc-pc (update state a - (reg-or-val state b))))

(defmethod command 'jnz [state [_ a b]]
  (if-not (zero? (reg-or-val state a))
    (update state 'pc + (reg-or-val state b))
    (inc-pc state)))

(defn part-1 [input]
  (let [inst (read-instructions input)]
    (loop [state start-state mul-count 0]
      (if-let [[op :as cmd] (get inst (state 'pc))]
        (recur
         (command state cmd)
         (if (= op 'mul) (inc mul-count) mul-count))
        mul-count))))


;; Code
#_"
set b 93         #     b = 93                  //b = 93
set c b          #     c = b                   //c = b
jnz a 2          #     a != 0 => goto l1       //if (!debug)
jnz 1 5          #     goto l2                 //   
                 # l1:                         //  
mul b 100        #     --                      //  
sub b -100000    #     b = (b * 100) + 100000  //  b = (b * 100) + 100000
set c b          #     --                      //  c = b + 17000
sub c -17000     #     c = b + 17000           //endif  
                 # l2:                         //while(true)
set f 1          #     f = 1                   //  f = 1
set d 2          #     d = 2                   //  
                 # l5:                         //  for (d = 2 to b)
set e 2          #     e = 2                   //    
                 # l4:                         //    for (e = 2 to b)
set g d          #     --                      //
mul g e          #     --                      //
sub g b          #     g = (d * e) - b         //      
jnz g 2          #     g != 0 => goto l3       //      if ((d * e) == b)
set f 0          #     f = 0                   //        f = 0
                 # l3:                         //      endif
sub e -1         #     e = e + 1               //      
set g e          #     --                      //
sub g b          #     g = e - b               //
jnz g -8         #     g != 0 => goto l4       //    endfor
sub d -1         #     d = d + 1               //    
set g d          #     --                      //
sub g b          #     g = d - b               //  
jnz g -13        #     g != 0 => goto l5       //  endfor
jnz f 2          #     f != 0 => goto l6       //  if (f == 0)
sub h -1         #     h = h + 1               //    res = res + 1
                 # l6:                         //  endif
set g b          #     --                      //
sub g c          #     g = b - c               //
jnz g 2          #     g != 0 => goto l7       //  if (b == c)
jnz 1 3          #     goto +3                 //    return res
                 # l7:                         //  endif
sub b -17        #     b = b + 17              //  b = b + 17
jnz 1 -23        #     goto l2                 //endwhile
"
;; Explanation:
;; This program counts the non prime numbers
;; from "b" to "c", with increments of 17

(defn is-prime? [n]
  (cond
    (= n 2) true
    (even? n) false
    :else (->> (range 3 (+ (Math/sqrt n) 1) 2)
               (some #(zero? (rem n %)))
               (not))))

(defn part-2 []
  (let [b (+ (* 93 100) 100000)
        c (+ b 17000)]
    (->> (range b (inc c) 17)
         (filter (complement is-prime?))
         (count))))

