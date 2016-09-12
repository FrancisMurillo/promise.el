;;; promise.el-test.el --- promise test
;;
;; Filename: promise.el-test.el
;; Description:
;; Author: Francis Murillo
;; Maintainer:
;; Created: Sat Aug 27 19:02:53 2016 (+0800)
;; Version:
;; Package-Requires: ()
;; Last-Updated:
;;           By:
;;     Update #: 0
;; URL:
;; Doc URL:
;; Keywords:
;; Compatibility:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:


(ert-deftest promise-then-fulfill-immediately ()
  (lexical-let* ((value 42)
      (operation (promise
                  (lambda (res rej)
                    (funcall res value)))))
    (should (eq 'fulfilled (promise--state operation)))
    (promise-then (lambda (the-value)
              (should (eq value the-value)))
            nil)))


(ert-deftest promise-then-reject-immediately ()
  (lexical-let* ((reason "Not enough mana")
      (operation (promise
                  (lambda (res rej)
                    (funcall rej reason)))))
    (should (eq 'rejected (promise--state operation)))
    (promise-then nil
            (lambda (the-reason)
              (should (eq reason the-reason))))))


(ert-deftest promise-then-immutable-state ()
  (lexical-let* ((final-value 'up)
      (operation (promise
                  (lambda (res rej)
                    (funcall res final-value)
                    (funcall rej 'down)
                    (funcall res 'left)))))
    (should (eq 'fulfilled (promise--state operation)))
    (promise-then (lambda (the-value)
              (should (eq final-value the-value)))
            (lambda (the-reason)
              (should-not the-reason)))))


(ert-deftest promise-then-delayed ()
  (lexical-let* ((delayed-value 1.618)
      (resolver nil)
      (operation (promise
                  (lambda (res rej)
                    (setq resolver res)))))
    (should (eq 'pending (promise--state operation)))
    (funcall resolver delayed-value)
    (should (eq 'fulfilled (promise--state operation)))
    (promise-then (lambda (the-value)
                    (shotuld (eq delayed-value the-value))))))

(ert-deftest promise-then-chain ()
  (letrec
      ((first-value 1)
       (first nil)
       (second nil)
       (third nil)
       (first-trigger nil)
       (first-part
        (promise
         (lambda (res _)
           (setq first t
                 first-trigger res))))
       (second-trigger nil)
       (second-value nil)
       (second-part
        (promise-then
         first-part
         (lambda (value)
           (setq second t
                 second-value (1+ value))
           (promise
            (lambda (res _)
              (setq second-trigger res))))))
       (third-trigger nil)
       (third-value nil)
       (third-part
        (promise-then
         second-part
         (lambda (value)
           (setq third t
                 third-value (1+ value))
           (promise
            (lambda (res _)
              (setq third-trigger res)))))))
    (should (eq first t))
    (should (null second))
    (should (null third))

    (funcall first-trigger first-value)

    (should (eq second t))
    (should (null third))
    (should (= (1+ first-value) second-value))

    (funcall second-trigger second-value)

    (should (eq third t))
    (should (= (1+ second-value) third-value))

    (funcall third-trigger third-value)

    (promise-then
     third-part
     (lambda (value)
       (should (= third-value third-value))))))


(ert-deftest promise-all-fulfilled ()
  (lexical-let* ((left-operation
                  (promise
                   (lambda (res rej)
                     (funcall res 'left))))
                 (right-operation
                  (promise
                   (lambda (res rej)
                     (funcall res 'right)))))
    (should (eq 'fulfilled (promise--state left-operation)))
    (should (eq 'fulfilled (promise--state right-operation)))
    (promise-then (promise-all left-operation right-operation)
            (lambda (values)
              (pcase-let ((`(,left ,right) values))
                (should (eq 'left left))
                (should (eq 'right right))))
            (lambda (reason)
              (should-not reason)))))

(ert-deftest promise-all-rejected ()
  (lexical-let* ((left-operation
                  (promise
                   (lambda (res rej)
                     (funcall res 'left))))
                 (right-operation
                  (promise
                   (lambda (res rej)
                     (funcall rej 'right)))))
    (should (eq 'fulfilled (promise--state left-operation)))
    (should (eq 'rejected (promise--state right-operation)))
    (promise-then (promise-all left-operation right-operation)
            (lambda (values)
              (should-not values))
            (lambda (reason)
              (should (eq 'right reason))))))


(ert-deftest promise-all-forever ()
  (lexical-let* ((left-operation
                  (promise
                   (lambda (res rej)
                     (funcall res 'left))))
                 (right-operation
                  (promise
                   (lambda (res rej)
                     nil))))
    (should (eq 'fulfilled (promise--state left-operation)))
    (should (eq 'pending (promise--state right-operation)))
    (promise-then (promise-all left-operation right-operation)
            (lambda (values)
              (should-not values))
            (lambda (reason)
              (should-not reason)))))

(ert-deftest promise-all-features ()
  (promise-then
   (promise-all-features 'ert 'promise)
   (lambda (_)
     (should t))))


(provide 'promise-test)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; promise-test.el ends here
