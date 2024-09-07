;;; SETUP-ABBREV ---   -*- lexical-binding: t; -*-
;;
;; Author: Marc Wenzlawski <marc.wenzlawski@icloud.com>
;; Copyright © 2024, Marc Wenzlawski, all rights reserved.
;; Created:  7 September 2024
;;
;;; Commentary:
;;
;;  
;;
;;; Code:

;; -*- coding: utf-8; lexical-binding: t; -*-
;; sample use of abbrev

(clear-abbrev-table global-abbrev-table)

(define-abbrev-table 'global-abbrev-table
  '(
    ;; ("name" "expansion")
    ))

;; define abbrev for specific major mode
;; the first part of the name should be the value of the variable major-mode of that mode
;; e.g. for go-mode, name should be go-mode-abbrev-table

(progn
  (when (boundp 'go-mode-abbrev-table)
    (clear-abbrev-table go-mode-abbrev-table))
  ;; (define-abbrev-table 'go-mode-abbrev-table
  ;;     '(("go" "package main
  ;; import \"fmt\"
  ;; func main() {
  ;;         fmt.Println(\"3\")
  ;; }")
  ;;       ("p" "fmt.Printf(\"%v\\n\", hh▮)")
  ;;       ("pl" "fmt.Println(hh▮)")
  ;;       ("r" "return")
  ;;       ("st" "string")
  ;;       ("eq" "==")
  ;;       ("v" "var x = 3")
  ;;       ("df" "x := 3")
  ;;       ("c" "const x = 3")
  ;;       ("f" "func ff(x int) int {
  ;;     return nil
  ;; }")
  ;;       ("if" "if 4 { 3 }")
  ;;       ("ie" " if err != nil { panic(err) }")
  ;;       ("ei" "else if x > 0 { 3 }")
  ;;       ("else" "else { 3 }")
  ;;       ("for" "for i := 0; i < 4; i++ { i }")
  ;;       ("fr" "for k, v := range xxx {
  ;; ▮
  ;;     }
  ;; ")))
  )

(set-default 'abbrev-mode t)

(setq save-abbrevs nil)

(provide 'setup-abbrev)
;;; setup-abbrev.el ends here
