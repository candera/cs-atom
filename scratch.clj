(def a-post (nth (content-by-author "craig-andera") 100))

(count (:tidied-body a-post))
(count (a-post "Body"))
(count (a-post "FormattedBody"))

(let [nodes (html-resource (StringReader. (a-post "Body")))
      ;nodes (remove-blank-p-tags nodes)
      ]
  (count (apply str
          (emit* (if-let [body (select nodes [:html :body content])]
                   body
                   nodes)))))


(pprint
 (select (html-resource (StringReader. (a-post "Body"))) [:html :body :> content]))
