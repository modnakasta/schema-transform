(ns com.intentmedia.schema-transform.json-transform-test
  (:require [cheshire.core :refer [generate-string]]
            [clojure.java.io :as io]
            [clojure.test :refer :all]
            [com.intentmedia.schema-transform.json-transform :refer :all]
            [schema.core :as s]))

(defn- read-schema [filename]
  (slurp (io/file (io/resource filename))))


(deftest test-primitive-types
  (are [json prismatic] (= prismatic (json-primitive->prismatic-primitive json))
    "boolean" s/Bool
    "integer" s/Int
    "number"  s/Num
    "string"  s/Str
    "null"    nil))


(deftest test-object-additional-properties
  (are [json prismatic] (= prismatic (json->prismatic (generate-string json)))
    {:type "object"}
    {s/Str s/Any}

    {:type "object"
     :additionalProperties false}
    {}

    {:type "object"
     :additionalProperties {:type "boolean"}}
    {s/Str s/Bool}))


(deftest test-array-schemas
  (testing "test uniform list"
    (are [json prismatic] (= prismatic (json->prismatic (generate-string json)))
      {:type "array"}
      [s/Any]

      {:type "array"
       :items {:type "number"}}
      [s/Num]))

  (testing "test tuple"
    (are [json prismatic] (= prismatic (json->prismatic (generate-string json)))
      {:type "array"
       :items [{:type "integer"} {:type "string"}]}
      [(s/optional s/Int "1") (s/optional s/Str "2") s/Any]

      {:type "array"
       :additionalItems false
       :items [{:type "integer"} {:type "string" :enum ["one" "two" "zero"]}]}
      [(s/optional s/Int "1") (s/optional (s/enum "one" "two" "zero") "2")])))


(deftest test-combinator-schemas
  (are [json prismatic] (= prismatic (json->prismatic (generate-string json)))
    {:allOf [{:type "object" :properties {:a {:type "integer"}}}
             {:type "object" :properties {:b {:type "string"}}}]}
    (s/both
      {(s/optional-key :a) s/Int
       s/Str               s/Any}
      {(s/optional-key :b) s/Str
       s/Str               s/Any})

    {:anyOf [{:type "string"} {:type "number"}]}
    (s/either s/Str s/Num)

    {:oneOf [{:type "string"} {:type "number"}]}
    (s/either s/Str s/Num)))


(deftest test-json-transform
  (testing "Converts a simple object type"
    (is (= {:order_id    s/Int
            :customer_id s/Int
            :total       s/Num}
          (json->prismatic (read-schema "simple.json")))))

  (testing "Converts schema with $ref"
    (let [adr {:street_address s/Str
               :city           s/Str
               :state          s/Str}]
      (is (= {(s/optional-key :billing_address)  adr
              (s/optional-key :shipping_address) adr}
             (json->prismatic (read-schema "refs.json"))))))

  (testing "Converts recursive schema"
    (let [schema (json->prismatic (read-schema "recursive.json"))
          tree   {:name     "root"
                  :children [{:name     "first"
                              :children [{:name     "subfirst"
                                          :children []}]}
                             {:name     "second"
                              :children nil}]}]
      (is (nil? (s/check schema tree)))
      (is (some? (s/check schema {:invalid :schema})))))

  (testing "Converts a complex object type"
    (is (= {:order_id    s/Int
            :customer_id s/Int
            :total       s/Num
            :order_details
            [{:quantity s/Int
              :total    s/Num
              :product_detail
              {:product_id                           s/Int
               :product_name                         s/Str
               (s/optional-key :product_description) (s/maybe s/Str)
               :product_status                       (s/enum "AVAILABLE" "OUT_OF_STOCK")
               :product_tags                         [s/Str]
               :price                                s/Num
               :product_properties                   {s/Str s/Str}}}]}
           (json->prismatic (read-schema "complex.json"))))))
