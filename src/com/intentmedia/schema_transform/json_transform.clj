(ns com.intentmedia.schema-transform.json-transform
  (:require [cheshire.core :refer [parse-string]]
            [clojure.string :as str]
            [schema.core :as s]))

(def ^:dynamic *ctx* nil)

(declare json-type-transformer)

(def json-primitive->prismatic-primitive
  {"boolean" s/Bool
   "integer" s/Int
   "number"  s/Num
   "string"  s/Str
   "null"    nil})


(defn predicates
  ([min max] (predicates min max nil))
  ([min max unique]
   (cond-> []
     (and min max)
     (conj (s/pred #(<= min (count %) max) (format "(<= %d size %d)" min max)))

     min
     (conj (s/pred #(<= min (count %)) (format "(<= %d size)" min)))

     max
     (conj (s/pred #(<= (count %) max) (format "(<= size %d)" max)))

     unique
     (conj (s/pred #(= % (distinct %)) "unique")))))


(defn add-preds [schema preds]
  (if (empty? preds)
    schema
    (apply s/both (cons schema preds))))


(defn json-object-props-transformer [json-object-type]
  (let [properties (:properties json-object-type)
        required   (->> json-object-type :required (map keyword) (into #{}))
        required?  (partial contains? required)]
    (->> properties
         (map
           (fn [[name schema]]
             (let [key-modifier (if (required? name)
                                  identity
                                  s/optional-key)]
               [(key-modifier name)
                (json-type-transformer schema)])))
         (reduce
           (fn [combiner [k v]]
             (assoc combiner k v))
           {}))))


(defn json-object-additional-props-transformer [transformed add-props]
  (cond
    (false? add-props)
    transformed

    (map? add-props)
    (assoc transformed s/Str (json-type-transformer add-props))

    :else
    (assoc transformed s/Str s/Any)))


(defn json-object-transformer [json-object-type]
  (let [add-props (:additionalProperties json-object-type)
        preds (predicates (:minProperties json-object-type) (:maxProperties json-object-type))]
    (-> (json-object-props-transformer json-object-type)
        (json-object-additional-props-transformer add-props)
        (add-preds preds))))


(defn json-tuple-transformer [json-array-type]
  (let [schema (->> (:items json-array-type)
                    (map-indexed #(s/optional (json-type-transformer %2) (str (inc %1))))
                    (into []))]
    (if (false? (:additionalItems json-array-type))
      schema
      (conj schema s/Any))))


(defn json-list-transformer [json-array-type]
  [(json-type-transformer (:items json-array-type))])


(defn json-array-transformer [json-array-type]
  (let [preds (predicates
                (:minItems json-array-type)
                (:maxItems json-array-type)
                (:uniqueItems json-array-type))]
    (-> (if (map? (:items json-array-type))
          (json-list-transformer json-array-type)
          (json-tuple-transformer json-array-type))
        (add-preds preds))))


(defn json-nil? [type]
  (= "null" type))


(defn ref? [json-type]
  (:$ref json-type))


(defn enum? [json-type]
  (vector? (:enum json-type)))


(def combinators (juxt :allOf :anyOf :oneOf))


(defn combinator? [json-type]
  (some vector? (combinators json-type)))


(defn nilable? [types]
  (some json-nil? types))


(defn union? [types]
  (and (vector? types)
       (> (count types) 0)))


(defn starts-with? [s prefix]
  (some-> s (.startsWith prefix)))


(defn get-json-schema [ref]
  (when-not (starts-with? ref "#/")
    (throw (ex-info (str "Can't parse ref. Only refs to current document are supported") {:ref ref})))
  (let [path (->> (rest (str/split ref #"/"))
                  (map keyword)
                  (into [:root]))]
    (get-in *ctx* path)))


(defn json-ref-transformer [json-ref-type]
  (let [ref    (:$ref json-ref-type)
        schema (get-in *ctx* [:defs ref])]
    (if schema
      schema
      (let [schema (json-type-transformer (get-json-schema ref))]
        (swap! (:defs *ctx*) assoc ref schema)
        schema))))


(defn json-enum-transformer [json-enum-type]
  (apply s/enum (:enum json-enum-type)))


(defn json-combinator-transformer [json-combinator-type]
  (let [[all any one] (combinators json-combinator-type)
        [fn schemas]  (cond
                        all [s/both all]
                        any [s/either any]
                        one [s/either one])]
    ;; TODO: oneOf should be exclusive
    (apply fn (map json-type-transformer schemas))))


(defn json-nilable-transformer [json-nilable-type]
  (let [types (into [] (remove json-nil? (:type json-nilable-type)))]
    (s/maybe (json-type-transformer
               (assoc json-nilable-type :type types)))))


(defn json-union-type-transformer [json-union-type]
  (let [types (:type json-union-type)]
    (if (= 1 (count types))
      (json-primitive->prismatic-primitive (first types))
      (apply s/cond-pre (map json-primitive->prismatic-primitive types)))))


(def json-type->transformer
  {"object" json-object-transformer
   "array"  json-array-transformer})


(defn json-type-transformer [json-type]
  (let [type (:type json-type)]
    (cond
      (nilable? type)
      (json-nilable-transformer json-type)

      (ref? json-type)
      (json-ref-transformer json-type)

      (enum? json-type)
      (json-enum-transformer json-type)

      (combinator? json-type)
      (json-combinator-transformer json-type)

      (union? type)
      (json-union-type-transformer json-type)

      (contains? json-primitive->prismatic-primitive type)
      (json-primitive->prismatic-primitive type)

      :else
      (let [transformer (json-type->transformer type)]
        (if transformer
          (transformer json-type)
          (throw (ex-info (str "No transformer for type " type) {:json-type json-type})))))))


(defn json-parsed->prismatic [json]
  (binding [*ctx* {:root json
                   :defs (atom {})}]
    (json-type-transformer json)))


(defn json->prismatic [json]
  (json-parsed->prismatic (parse-string json true)))
