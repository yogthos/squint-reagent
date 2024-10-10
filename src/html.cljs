(ns html
  (:require
   [clojure.string :as string]
   [clojure.walk :as w]))
  
(defn keyword->str [kw]
  (if (keyword? kw)
    (name kw)
    kw))

(defn primitive? [hiccup]
  (or (string? hiccup)
      (number? hiccup)
      (keyword? hiccup)
      (boolean? hiccup)
      (inst? hiccup)
      (nil? hiccup)
      (char? hiccup)))

(defn component? [c]
  (and (sequential? c)
       (let [first-element (first c)]
         (or (fn? first-element)
             (map? first-element)))))

(defn normalize-hiccup
  "hiccup should be in form of [:tag {} body]"
  [hiccup]
  (let [hiccup (w/postwalk #(if (nil? %) "" %) hiccup)
        [tag maybe-attr-map & body] hiccup
        nh (cond
             (= 1 (count hiccup))
             (conj hiccup {})

             (and (not (map? maybe-attr-map))
                  (nil? body))
             (let [element maybe-attr-map]
               [tag {} element])

             (and (not (map? maybe-attr-map)))
             (let [element maybe-attr-map]
               (into [tag {} element] body))
             
             :else hiccup)]
    nh))

(defn extract-tag-id-css-classes [tag-maybe-id-css-classes]
  (let [as-str (keyword->str tag-maybe-id-css-classes)
        chunks (string/split as-str #"\.")
        tag-with-maybe-id (first chunks)
        [tag id] (string/split tag-with-maybe-id #"#")
        css-classes (rest chunks)]
    [tag id css-classes]))

(defn style-map->css-str [m]
  (->> m
       (map (fn [[k v]]
              (let [css-key (keyword->str k)
                    css-val (cond
                              (number? v) (str v "px")
                              :else (keyword->str v))]
                (str css-key ": " css-val))))
       (string/join "; ")))

(defn- attribute-map->str [attribute-map]
  (when (seq attribute-map)
    (string/join " " (for [[k v] attribute-map]
                       (if (= k :style)
                         (str "style=\"" (style-map->css-str v) "\"")
                         (str (keyword->str k) "=\"" v "\""))))))

(declare hiccup->html-str)

(defn- create-html-str [tag-maybe-id-css-classes attr-map hiccup-content]
  (let [[tag-name id css-classes] (extract-tag-id-css-classes tag-maybe-id-css-classes)
        attribute-map (if (empty? css-classes)
                        attr-map
                        (assoc attr-map :class (string/join " " css-classes)))
        attribute-map (if (nil? id)
                        attribute-map
                        (assoc attribute-map :id id))
        attributes (attribute-map->str attribute-map)
        nested? (and (sequential? hiccup-content)
                     (= 1 (count hiccup-content)))
        hiccup-content (if nested?
                         (first hiccup-content)
                         hiccup-content)]
    (cond
      (or (= tag-name "link")
          (= tag-name "input")) (str "<" tag-name " " attributes ">")

      :else (let [begin-tag (cond
                              (and tag-name attributes) (str "<" tag-name " " attributes ">")
                              tag-name                  (str "<" tag-name ">"))

                  body (->> hiccup-content hiccup->html-str)
                  end-tag  (str "</" tag-name ">")
                  well-formed [begin-tag body end-tag]
                  html-str (string/join "" well-formed)]
              html-str))))

(declare  component-node->hiccup)

(defn fn->hiccup [render-fn params]
  (let [component-or-hiccup (apply render-fn params)
        hiccup (if (component? component-or-hiccup)
                 (component-node->hiccup component-or-hiccup)
                 (normalize-hiccup component-or-hiccup))]
    (w/prewalk component-node->hiccup
               hiccup)))

(defn hiccup->html-str [hiccup]
  (if (primitive? hiccup)
    (str hiccup)
    (let [first-element (first hiccup)
          second-element (second hiccup)]
      (cond
        (and (keyword? first-element)
             (map? second-element)) (let [tag first-element
                                          attr-map second-element
                                          content (drop 2 hiccup)]
                                      (create-html-str tag attr-map content))
        (keyword? first-element) (let [tag first-element
                                       content (rest hiccup)]
                                   (create-html-str tag {} content))
        (fn? first-element) (let [func first-element
                                  params (rest hiccup)
                                  h (fn->hiccup func params)]
                              (hiccup->html-str h))
        (map? first-element) (let [func (:reagent-render first-element)
                                   params (rest hiccup)
                                   h (fn->hiccup func params)]
                               (hiccup->html-str h))
        (every? empty? hiccup) ""
        :else (string/join " " (map hiccup->html-str hiccup))))))

(defn component-node->hiccup [node]
  (if (component? node)
    (let [render-fn (let [first-element (first node)]
                      (cond
                        (fn? first-element) first-element
                        (map? first-element) (:reagent-render first-element)))
          params (rest node)
          ;;TODO: should check mounted-components first
          hiccup (fn->hiccup render-fn params)]
      hiccup)
    node))

(defn component->hiccup [[{:keys [reagent-render]} & params :as normalized-component]]
  (let [hiccup (fn->hiccup reagent-render params)
        hiccup (w/prewalk component-node->hiccup
                          hiccup)]
    hiccup))

(declare hiccup->dom)

(defn fn->dom [render-fn params]
  (let [hiccup (fn->hiccup render-fn params)]
    (hiccup->dom hiccup)))

(defn add-css-to-element [element css-classes]
  (let [css-classes (remove #(or (nil? %) (string/blank? %)) css-classes)]
    (when-not (empty? css-classes)
      (doseq [c css-classes]
        (.. element -classList (add c))))))

(defn create-element [tag-maybe-id-css-classes attr-map hiccup-content]
  (let [[tag-name id css-classes] (extract-tag-id-css-classes tag-maybe-id-css-classes)
        element (js/document.createElement tag-name)
        element-children (hiccup->dom hiccup-content)
        nested? (and (seq? element-children)
                     (every? seq? element-children))
        element-children (if nested?
                           (mapcat identity element-children)
                           element-children)]
    (when id
      (set! (.-id element) id))

    (add-css-to-element element css-classes)
    (when-not (empty? attr-map)
      (doseq [[k v] attr-map
              :let [k-str (keyword->str k)]]
        (cond
          (= k :style) (.. element (setAttribute "style" (style-map->css-str v)))
          (= k :class) (let [css-classes (if (string? v)
                                           (string/split v #"\s+")
                                           (map name v))]
                         (add-css-to-element element css-classes))
          (re-find #"on-\w+-*\w+" k-str) (let [event (string/replace k-str "on-" "")
                                               event (string/replace event "-" "")]
                                           (.. element (addEventListener event v)))
          :else (let [v-str (keyword->str v)]
                  (.. element (setAttribute k-str v-str))))))

    (if (seq? element-children)
      (doseq [child element-children]
        (.. element (appendChild child)))
      (.. element (appendChild element-children)))
    element))

(defn hiccup->dom [hiccup]
  (if (primitive? hiccup)
    (js/document.createTextNode hiccup)
    (let [first-element (first hiccup)
          second-element (second hiccup)]
      (cond
        (and (keyword? first-element)
             (map? second-element)) (let [tag first-element
                                          attr-map second-element
                                          content (rest (rest hiccup))]
                                      (create-element tag attr-map content))
        (keyword? first-element) (let [tag first-element
                                       content (rest hiccup)]
                                   (create-element tag {} content))
        (fn? first-element) (let [func first-element
                                  params (rest hiccup)]
                              (fn->dom func params))
        (map? first-element) (let [func (:reagent-render first-element)
                                   params (rest hiccup)]
                               (fn->dom func params))
        :else (map hiccup->dom hiccup)))))

(defn node->hiccup [node hiccup]
  (let [tag-name (-> node .getName keyword)
        children (.. node getAllChildren)]
    (cond
      tag-name
      (into [tag-name] (if (seq children)
                         (remove (fn [e]
                                   (when (and (string? e)
                                              (string/blank? e))
                                     true))
                                 (map (fn [a-node]
                                        (if (= (type a-node)
                                               org.htmlcleaner.ContentNode)
                                          (.. a-node getContent)
                                          (node->hiccup a-node (conj hiccup (let [tag-name (-> a-node .getName keyword)]
                                                                              tag-name)))))
                                      children))

                         hiccup))
      :else
      (if (seq children)
        (map (fn [a-node]
               (node->hiccup a-node hiccup))
             children)

        hiccup))))

(defn replace-node [new-node current-node]
  (let [parent (.-parentNode current-node)]
    (.replaceChild parent new-node current-node)))

(defn append-child [parent child]
  (.appendChild parent child))

(defn remove-node [dom-node]
  (let [parent (.-parentNode dom-node)]
    (.removeChild parent dom-node)))

(defn remove-children [element]
  (while (.-firstChild element)
    (.removeChild element (.-firstChild element))))
