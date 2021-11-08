(ns users.core
  (:require [monger.core :as mg]
            [monger.collection :as mc]
            [meta-csv.core :as csv]
            [clojure.java.io :as io])
  (:import org.bson.types.ObjectId))


(defmacro with-try-catch
  [& body]
  `(try
     ~@body
     (catch Throwable t# (println (str "caught exception: " (.toString t#))))))         

(defn initialize []
  (def conn (mg/connect))
  (def db (mg/get-db conn "usersdb"))
  (def coll "users"))

(defn reader_function [command]
  (println command ": ")
  ;; (def console_input (read-line))
  ;; console_input)

(let [console_input (read-line)]
  console_input)) 
     
(defn show_contacts [level]
  (def db_dump (mc/find-maps db coll))
  (def length_of_list (count db_dump))
  (def i (atom 0))
  (println "ID \t name \t phone \t \t privilege")
  (while (< @i length_of_list)
    (do
       (let [useri (nth db_dump @i)
          namei ((select-keys useri [:name]) :name)
         phonei ((select-keys useri [:phone]) :phone)
         idi ((select-keys useri [:ID]) :ID)
        ;;  i_id ((select-keys useri [:_id]) :_id)
         prevli ((select-keys useri [:prevl]) :prevl)
         passi ((select-keys useri [:password]) :password)]
         (case level
           1 (println idi "\t" namei "\t" phonei "\t" prevli "\t" passi)
           2 (println idi "\t" namei "\t" phonei "\t" prevli)
           3 (println idi "\t" namei "\t" phonei)
           (println "you are not authorized to view any details")))
       (swap! i inc))))

(defn adding_to_db [add_db_access_level]
  (let [new_name (reader_function "username")
        user_id (reader_function "user ID")
        user_privil (Integer/parseInt (reader_function "privilege"))
        user_phone (reader_function "Phone Number")
        user_pass (reader_function "Password")]
    (if (< user_privil add_db_access_level)
      (println "you don't have authority to add higher privileged user")
  ;; (mc/insert-and-return db coll {:name new_name :ID user_id :prevl user_privil :phone user_phone :password user_pass})
      (mc/insert db coll {:_id (ObjectId.) :name new_name :ID user_id :prevl user_privil :phone user_phone :password user_pass}))))

(defn deleting_from_db [delete_db_access_level]
  (def delete_name (reader_function "Which user do you wanna delete ?"))

  (let [lookup_doc (mc/find-maps db coll {:name delete_name})]
  ;; (def doc_empty (empty? lookup_doc))
  (if (not (empty? lookup_doc))
    (do
      (def lookup_prevl ((select-keys (first lookup_doc) [:prevl]) :prevl))
      (if (= lookup_prevl 1)
        (println "you cannot delete higher privileged user")
        (mc/remove db coll {:name delete_name})))
    (println "There is no such user"))))

(defn add_contacts [add_access_level]
  (if (or (= add_access_level 1) (= add_access_level 2))
  (do (adding_to_db add_access_level) (show_contacts add_access_level))
  (println "You don't have the privilege")))

(defn delete_contacts[delete_access_level]
  (if (or (= delete_access_level 1) (= delete_access_level 2))
    (do (deleting_from_db delete_access_level) 
        (show_contacts delete_access_level))
    (println "You don't have the privilege")))

(defn helping []
  (println "Available options")
  (println "[add] [show] [del]  [lookup] [logout][help]"))

(defn login_check [username_attempt]
  ;; (def matching_doc (mc/find-maps db coll {:name username_attempt}))
  ;; (def matching_name ((select-keys (first matching_doc) [:name]) :name))
  ;; (def matching_pass ((select-keys (first matching_doc) [:password]) :password))
  ;; (def matching_prevl ((select-keys (first matching_doc) [:prevl]) :prevl))
  ;; [matching_name matching_pass matching_prevl]
  (let [matching_doc (mc/find-maps db coll {:name username_attempt})]
  [((select-keys (first matching_doc) [:name]) :name) 
   ((select-keys (first matching_doc) [:password]) :password) 
   ((select-keys (first matching_doc) [:prevl]) :prevl)]))

(defn look_up_by_name [lookup_access_level]
  (def username_lookup (reader_function "Search for user:"))
  (def lookup_doc (mc/find-maps db coll {:name username_lookup}))
  ;; (def doc_empty (empty? lookup_doc))
  (if (not (empty? lookup_doc))
    (do
      (def lookup_name ((select-keys (first lookup_doc) [:name]) :name))
      (def lookup_phone ((select-keys (first lookup_doc) [:phone]) :phone))
      (def lookup_id ((select-keys (first lookup_doc) [:ID]) :ID))
      (println "User:" lookup_name "\nUser ID:" lookup_id "\nContact:" lookup_phone)
      (if (<= lookup_access_level 2)
        (println "Privilege:" ((select-keys (first lookup_doc) [:prevl]) :prevl)))
      (if (<= lookup_access_level 1)
        (println "Password:" ((select-keys (first lookup_doc) [:password]) :password))))
    (println "there is no such user"))
  (println "------------------------------------------------------------"))

;; (defn export_user[]
;;   (def username_lookup (reader_function "Search for user:"))
;;   (def lookup_doc (mc/find-maps db coll {:name username_lookup}))
;;   ;; (def doc_empty (empty? lookup_doc))
;;   (if (not (empty? lookup_doc))
;;     (do
;;       (def doc_name ((select-keys (first lookup_doc) [:name]) :name))
;;       (println lookup_doc)
;;       (let [[uid uname ucontact uprvl upass] lookup_doc])
;;       ((with-open [wrtr (io/writer (str "./resources/" doc_name ".txt") :append true)] (doseq [i saving_data] (.write wrtr (str i "\n"))))))
;;     )
;;   )

(defn read_from_file [prvil]
  (println "reading from file")
  (if (< prvil 2)
    (do
      (def file_data (csv/read-csv "./resources/tem.csv"))
      (def rows (count file_data))
      (let [i (atom 0)]
        (while (< @i rows)
           (do
             (let [[f_uid f_name f_contact f_prvl f_pass] (nth file_data @i)]
               (println "UserID : " f_uid "Name : " f_name "Contact : " f_contact "Access Level : " f_prvl "Address : " f_pass)
               (mc/insert db coll {:_id (ObjectId.) :name f_name :ID f_uid :prevl f_prvl :phone f_contact :password f_pass})))
          (swap! i inc))))
    (println "you don't have the access to read from a file")))

(defn document[]
  (-> (slurp "./resources/doc.txt")
      (println))
)

(defn save_to_doc []
  ;; (def docs (reader_function "input the line you need to save"))
  ;; (spit "./resources/doc.txt" "New line added" :append true)
  (with-open [wrtr (io/writer "./resources/doc.txt" :append true)]
    (doseq [i `["a" "abc" "qwe"]]
      (.write wrtr (str i "\n"))))
  )
  

(defn user_app[]
(def isAuth (atom 0))
  (def privilege (atom 0))
  (println "-----------------------userApp------------------------------")
  (def loginID (reader_function "loginID"))
  (def passID (reader_function "Password"))
  (def mongo_return (login_check loginID))
  (def user_name (nth mongo_return 0))
  (def password (nth mongo_return 1))
  (if (and (= loginID user_name) (= passID password))
      (do (reset! isAuth 1)
          (reset! privilege (nth mongo_return 2))
          (println "Hi"user_name", you are logged in successfully - Access Level"@privilege))
      (do (println "Invalid username or password")))

  (while (= @isAuth 1)
    (do
      (def word (reader_function "Enter your command"))
      (if (= word "logout")
        (do (println "[User: "user_name "][Access Level: "@privilege "] [help to view options]")
          (reset! isAuth 0) (println "logging out") (println "------------------------------------------------------------"))
        (do (println "[User:"user_name"][Access Level:"@privilege"] [help to view options]")
            (println "------------------------------------------------------------")
            (case word
              "show" (with-try-catch (show_contacts @privilege))
              "add" (with-try-catch add_contacts @privilege)
              "del" (delete_contacts @privilege)
              "help" (helping)
              "lookup" (look_up_by_name @privilege)
              "file" (read_from_file @privilege)
              "doc" (document)
              "save" (save_to_doc)
              ;; "exp" (with-try-catch (export_user))
              (println "Enter valid commands")))))))

(defn -main
  "main"
  [& args]
  (initialize)
  (read-line)
  (def continue (atom "y"))
  (while (not= @continue "n")
    (do 
      (user_app)
      (def yn (reader_function "Do you want to continue(y/n)?"))
      (reset! continue yn))
      )
  (println  "-------------------Exiting Users App------------------------"))
 