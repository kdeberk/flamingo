(ns flamingo.core)

(defrecord Graph [current-state state-counter transitions])

(defn make-graph []
  (Graph. :start 0 []))

(defrecord Label [name fields])
(defrecord InstantiatedLabel [label values])
(defrecord Transition [from to send receive constraint update])

;; TODO: updates become functions that take a valuation and (optionally) a label. They return a valuation with the new label.
;; TODO: constraints become functions that take a valuation and return a bool. Their variables are resolved to known variables within the valuation.

(defn make-transition [& {:keys [from to send receive constraint update]}]
  (Transition. from to send receive constraint update))

(defn expr-type [expr] (when (list? expr) (first expr)))
(defmulti parse-expr (fn [_ expr] (expr-type expr)))

(defmethod parse-expr 'if [graph expr]
  (let [[graph start end] (create-fork graph)]
    (let [[_ cond then else] expr]
      (-> graph
          (start-branch start)
          (add-transition (make-transition :constraint cond))
          (parse-expr then)
          (stop-branch end)
          (start-branch start)
          (add-transition (make-transition :constraint (list 'not cond)))
          (parse-expr else)
          (stop-branch end)))))


(defmethod parse-expr 'select [graph expr]
  (let [[graph start end] (create-fork graph)]
    (reduce (fn [graph expr]
                    (-> graph
                        (start-branch start)
                        (parse-expr expr)
                        (stop-branch end)))
            graph
            (rest expr))))

(defmethod parse-expr 'receive [graph expr]
  (let [[_ label-expr] expr]
    (add-transition graph (make-transition :receive label-expr))))

(defmethod parse-expr 'send [graph expr]
  (let [[_ label-expr] expr]
    (add-transition graph (make-transition :send label-expr))))

(defmethod parse-expr 'loop [graph expr]
  (let [[_ & body] expr
        {start :current-state} graph
        graph (reduce parse-expr graph body)]
    (let [{transitions :transitions
           current-state :current-state} graph]
      (assoc graph
             :transitions (conj transitions (make-transition :from current-state :to start))
             :current-state nil))))

(defmethod parse-expr 'let [graph expr]
  (let [[_ bindings & body] expr]
    (let [bindings (apply hash-map bindings)
          var-names (map first bindings)]
      (as-> graph graph
        (reduce (fn [graph binding]
                  (let [[var expr] binding]
                    (add-transition graph
                                    (do
                                      (prn (list? expr) (= 'receive (first expr)) (first expr))
                                      (if (and (list? expr) (= 'receive (first expr)))
                                        (make-transition :receive(rest expr) :update (make-label-update var {}))
                                        (make-transition :update (make-update {var expr})))))))
                graph
                bindings)
        (reduce parse-expr graph body)
        (add-transition graph (make-transition :update (make-update (map #(vector % `(pop ~%)) var-names))))))))

(defmethod parse-expr nil [graph expr]
  graph)


;; Graph operations

(defn add-transition [graph transition]
  (let [{old-state :current-state
         transitions :transitions} graph]
    (let [next-state (inc (:state-counter graph))
          transition (assoc transition :from old-state :to next-state)]
      (assoc graph
             :state-counter next-state
             :current-state next-state
             :transitions (conj transitions transition)))))

(defn create-fork [graph]
  (let [{start-state :current-state} graph
        end-state (inc (:state-counter graph))]
    [(assoc graph :state-counter end-state)
     start-state
     end-state]))

(defn start-branch [graph from]
  (let [{transitions :transitions} graph
        state-counter (inc (:state-counter graph))]
    (assoc graph
           :state-counter state-counter
           :current-state state-counter
           :transitions (conj transitions (make-transition :from from :to state-counter)))))

(defn stop-branch [graph to]
  (let [{transitions :transitions} graph]
    (assoc graph
           :current-state to
           :transitions (conj transitions (make-transition :from (:current-state graph) :to to)))))

(defn reduce-graph [graph]
  (let [from-and-tos (merge-with list
                                 (group-by :to (:transitions graph))
                                 (group-by :from (:transitions graph)))
        candidate (first (filter (fn [[_ v]]
                                   (when (and (list? v))
                                     (let [[v0 v1] v]
                                       (and (= 1 (count v0))
                                            (= 1 (count v1))
                                            (can-merge-transitions (first v0) (first v1))))))
                                 from-and-tos))]
    (if candidate
      (let [[[from] [to]] (val candidate)]
        (-> graph
            (update :transitions (partial remove #{from to}))
            (update :transitions conj (merge-transitions from to))
            recur))
      graph)))

(defn can-merge-transitions [a b]
  (and (= (:to a) (:from b))
       (or (nil? (:send a)) (nil? (:send b)))
       (or (nil? (:receive a)) (nil? (:receive b)))
       (or (nil? (:constraint a)) (nil? (:constraint b)))
       (or (nil? (:update a)) (nil? (:update b)))))

(defn merge-transitions [a b]
  (Transition. (:from a) (:to b)
               (or (:send a) (:send b))
               (or (:receive a) (:receive b))
               (or (:constraint a) (:constraint b))
               (or (:update a) (:update b))))

;; Constructing functions

(defn make-update [bindings]
  (let [val-sym (gensym 'val-)]
    `(fn [~val-sym]
       (assoc ~val-sym ~@(mapcat (fn [[k v]]
                                   (let [key (keyword k)]
                                     [key (replace-symbols {'k (list key val-sym)} v)]))
                                 bindings)))))

(defn make-label-update [label-name bindings]
  (let [val-sym (gensym 'val-)
        lbl-sym (gensym 'lbl-)]
    (let [bindings (conj bindings [label-name lbl-sym])]
      `(fn [~lbl-sym ~val-sym]
         (assoc ~val-sym ~@(mapcat (fn [[k v]] [(keyword k) (replace-symbols {'k (list (keyword k) val-sym)} v)]) bindings))))))

(defn replace-symbols [m expr]
  (cond
    ;; On lets we try to follow the binding rules.
    (and (list? expr) (= 'let (first expr)))
    (let [[_ bindings & body] expr
          [m bindings] (reduce (fn [[m bindings] [val expr]]
                                 (if (m val)
                                   [(dissoc m val)
                                    (conj bindings val (replace-symbols m expr))]
                                   [m
                                    (conj bindings val (replace-symbols m expr))]))
                               [m []]
                               (partition 2 bindings))]
      `(let ~(into [] bindings)
         ~@(map (partial replace-symbols m) body)))
    ;; For map, recurse on the values.
    (map? expr)
    (into {} (map (fn [[k v]] [k (replace-symbols m v)]) expr))
    ;; For lists, recurse on every value.
    (seq? expr)
    (map (partial replace-symbols m) expr)
    ;; For vectors, also recurse on every value.
    (vector? expr)
    (into [] (map (partial replace-symbols m) expr))
    ;; We only replace symbols.
    (and (symbol? expr) (m expr))
    (m expr)
    ;; Nothing else matched.
    :else
    expr
    ))
