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
      (operation (p-make-promise
                  (lambda (res rej)
                    (funcall res value)))))
    (should (eq 'fulfilled (p--state operation)))
    (p-then (lambda (the-value)
              (should (eq value the-value)))
            nil)))


(ert-deftest promise-then-reject-immediately ()
  (lexical-let* ((reason "Not enough mana")
      (operation (p-make-promise
                  (lambda (res rej)
                    (funcall rej reason)))))
    (should (eq 'rejected (p--state operation)))
    (p-then nil
            (lambda (the-reason)
              (should (eq reason the-reason))))))


(ert-deftest promise-then-immutable-state ()
  (lexical-let* ((final-value 'up)
      (operation (p-make-promise
                  (lambda (res rej)
                    (funcall res final-value)
                    (funcall rej 'down)
                    (funcall res 'left)))))
    (should (eq 'fulfilled (p--state operation)))
    (p-then (lambda (the-value)
              (should (eq final-value the-value)))
            (lambda (the-reason)
              (should-not the-reason)))))


(ert-deftest promise-then-delayed ()
  (lexical-let* ((delayed-value 1.618)
      (resolver nil)
      (operation (p-make-promise
                  (lambda (res rej)
                    (setq resolver res)))))
    (should (eq 'pending (p--state operation)))
    (funcall resolver delayed-value)
    (should (eq 'fulfilled (p--state operation)))
    (p-then (lambda (the-value)
              (should (eq delayed-value the-value))))))


(ert-deftest promise-all-fulfilled ()
  (lexical-let* ((left-operation
                  (p-make-promise
                   (lambda (res rej)
                     (funcall res 'left))))
                 (right-operation
                  (p-make-promise
                   (lambda (res rej)
                     (funcall res 'right)))))
    (should (eq 'fulfilled (p--state left-operation)))
    (should (eq 'fulfilled (p--state right-operation)))
    (p-then (p-all left-operation right-operation)
            (lambda (values)
              (pcase-let ((`(,left ,right) values))
                (should (eq 'left left))
                (should (eq 'right right))))
            (lambda (reason)
              (should-not reason)))))

(ert-deftest promise-all-rejected ()
  (lexical-let* ((left-operation
                  (p-make-promise
                   (lambda (res rej)
                     (funcall res 'left))))
                 (right-operation
                  (p-make-promise
                   (lambda (res rej)
                     (funcall rej 'right)))))
    (should (eq 'fulfilled (p--state left-operation)))
    (should (eq 'rejected (p--state right-operation)))
    (p-then (p-all left-operation right-operation)
            (lambda (values)
              (should-not values))
            (lambda (reason)
              (should (eq 'right reason))))))


(ert-deftest promise-all-forever ()
  (lexical-let* ((left-operation
                  (p-make-promise
                   (lambda (res rej)
                     (funcall res 'left))))
                 (right-operation
                  (p-make-promise
                   (lambda (res rej)
                     nil))))
    (should (eq 'fulfilled (p--state left-operation)))
    (should (eq 'pending (p--state right-operation)))
    (p-then (p-all left-operation right-operation)
            (lambda (values)
              (should-not values))
            (lambda (reason)
              (should-not reason)))))

(ert-deftest promise-all-features ()
  (p-then
   (p-all-features 'ert 'promise)
   (lambda (_)
     (should t))))


(provide 'promise-test)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; promise-test.el ends here
