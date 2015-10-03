(ns com.intentmedia.schema-transform.avro-transform-test
  (:require [com.intentmedia.schema-transform.avro-transform :refer :all]
    [clojure.test :refer :all]
    [schema.core :as s]
    [clojure.java.io :as io]
    [cheshire.core :refer [parse-string]]))

;; Unit tests
(deftest test-avro-primitive-transformer
  (testing "Converts a single field"
    (is (= String (avro-primitive-transformer "string")))
    (is (= Integer (avro-primitive-transformer "int")))
    (is (= Double (avro-primitive-transformer "double")))
    (is (= Float (avro-primitive-transformer "float")))
    (is (= Long (avro-primitive-transformer "long")))
    (is (= Boolean (avro-primitive-transformer "boolean")))
    (is (= String (avro-primitive-transformer "bytes"))))
  (testing "Converts a union (nullable) field in either order"
    (is (= (s/maybe Integer) (avro-primitive-transformer ["int" "null"])))
    (is (= (s/maybe Integer) (avro-primitive-transformer ["null" "int"])))))

(deftest test-avro-array-transformer
  (testing "Converts an array"
    (is (= [String] (avro-array-transformer {:type  "array"
                                             :items "string"})))))

(deftest test-avro-enum-transformer
  (testing "Converts an enum"
    (is (= (s/enum "SPADES" "HEARTS" "DIAMONDS" "CLUBS")
          (avro-enum-transformer {:type    "enum"
                                  :symbols ["SPADES" "HEARTS" "DIAMONDS" "CLUBS"]})))))

(deftest test-avro-map-transformer
  (testing "Converts a map"
    (is (= {String Double} (avro-map-transformer {:type   "map"
                                                  :values "double"})))))

(deftest test-avro-record-transformer
  (testing "Converts a record"
    (is (= {:name            String
            :favorite_number (s/maybe Integer)}
          (avro-record-transformer {:name      "rec"
                                    :namespace "example.avro"
                                    :type      "record"
                                    :fields    [{:name "name" :type "string"}
                                                {:name "favorite_number" :type ["null" "int"]}]})))))

;; Integration Tests
(defn- get-schema [filename]
  (let [schema (slurp (io/file (io/resource filename)))
        ;_ (println (str "Testing: " schema))
        ]
    (parse-string schema true)))

(deftest test-avro-transform
  (testing "Converts a simple record type"
    (is (= {:order_id Integer
            :customer_id Integer
            :total Float}
          (avro->prismatic (get-schema "simple.avsc")))))
  (testing "Converts a complex record type"
    (is (= {:order_id Integer
            :customer_id Integer
            :total Float
            :order_details [{:quantity Integer
                             :total Float
                             :product_detail {:product_id Long
                                              :product_name String
                                              :product_description String
                                              :product_tags [String]
                                              :price Float
                                              :product_properties {String String}}}]}
          (avro->prismatic (get-schema "complex.avsc"))))))
