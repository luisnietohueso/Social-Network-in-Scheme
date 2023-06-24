
(module sn-graph scheme
  (provide sn-consistent
           sn-empty
           sn-add-user
           sn-users
           sn-add-frndshp)

  (require scheme/set
           scheme/dict)
 (define my-dict
  (list
   (cons 'f2
         (list 'f3 'f4))
   (cons 'f3
         (list 'f2))
   (cons 'f4
         (list 'f3 'f2))
   (cons 'f13
         (list ))
   (cons 'f1
         (list ))
   ))
;; sn-empty : -> empty
;; Returns an empty graph.
  (define sn-empty empty)
  
;;This function takes a graph as input and returns a list of all the users in the graph.
;; sn-users : (dict-of symbol any) -> list-of-symbol
;; Given a graph `graph`, returns a list of all the users in the graph.

  (define (sn-users graph)
    (dict-keys graph))
  
;;This function takes a graph and a user as input, and adds the user to the graph if they are not already in it.
;; sn-add-user : (dict-of symbol any) symbol -> (dict-of symbol any)
  (define (sn-add-user graph user)
    (if (dict-ref graph user #f)
        graph
        (dict-set graph user empty)))
  
;; This function takes a graph and two users as input, and adds a friendship between the two users. It does this by adding each user to the other's friend list in the graph.
;; sn-add-frndshp : (dict-of symbol (set-of symbol)) symbol symbol -> (dict-of symbol (set-of symbol))
;; Given a graph `graph` and two users `u1` and `u2`, adds a friendship between the two users.
;; It does this by adding each user to the other's friend list in the graph.
  (define (sn-add-frndshp graph u1 u2)
    (let ([f1 (dict-ref graph u1 empty)]
          [f2 (dict-ref graph u2 empty)])
      (dict-set graph
                u1
                (set-add f1 u2))
      (dict-set graph
                u2
                (set-add f2 u1))))
;;This function takes a graph as input and returns #t if the graph is consistent, i.e., if for each user in the graph, all of their friends are also in the graph.
;;The function uses a recursive helper function to check each user in the graph and their friends.
;; all of their friends are also in the graph.
;; sn-consistent : (dict-of symbol (set-of symbol)) -> boolean
  (define (sn-consistent graph)
    (define (consistent-helper [to-check (dict-keys graph)])
      (cond
        [(empty? to-check) #t]
        [else
         (let ([current-user (first to-check)]
               [remaining-users (rest to-check)])
           (define (friend-list-incomplete? user)
             (ormap (lambda (friend)
                      (and (dict-ref graph user #f)
                           (set-member? (dict-ref graph friend #f) user)))
                    (dict-ref graph user empty)))
           (if (friend-list-incomplete? current-user)
               #f
               (consistent-helper remaining-users)))]))
    (consistent-helper (dict-keys graph))))

