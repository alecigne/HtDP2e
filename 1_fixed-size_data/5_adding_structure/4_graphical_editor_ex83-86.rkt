;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname graphical_editor_ex83-86) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
; A graphical editor
; Exercises 83-85
; TODO: Exercise 86

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Constants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define TEXT-SIZE 30)
(define TEXT-COLOR "black")
(define CURSOR-COLOR "red")
(define ALIGN "left")
(define SCN (empty-scene 200 (+ TEXT-SIZE 20)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Data definitions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-struct editor [pre post])
; An Editor is a structure:
;   (make-editor String String)
; Interpretation: (make-editor s t) describes an editor
; whose visible text is (string-append s t) with 
; the cursor displayed between s and t.

(define ED1 (make-editor "hello " "world"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rendering function
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; render: Editor -> Image
; Consumes an Editor e and produces an image.

(check-expect (render (make-editor "hello " "world"))
              (overlay/align ALIGN "center"
                             (beside
                              (text "hello " TEXT-SIZE TEXT-COLOR)
                              (rectangle 1 TEXT-SIZE "solid" CURSOR-COLOR)
                              (text "world" TEXT-SIZE TEXT-COLOR))
                             SCN))

(check-expect (render (make-editor "" "test"))
              (overlay/align ALIGN "center"
                             (beside
                              (text "" TEXT-SIZE TEXT-COLOR)
                              (rectangle 1 TEXT-SIZE "solid" CURSOR-COLOR)
                              (text "test" TEXT-SIZE TEXT-COLOR))
                             SCN))

(define (render ed)
  (overlay/align ALIGN "center"
                 (beside
                  (text (editor-pre ed) TEXT-SIZE TEXT-COLOR)
                  (rectangle 1 TEXT-SIZE "solid" CURSOR-COLOR)
                  (text (editor-post ed) TEXT-SIZE TEXT-COLOR))
                 SCN))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helpers for strings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; string-remove-last: String -> String
; Deletes the last character from string str.

(check-expect (string-remove-last "hello") "hell")
(check-expect (string-remove-last "h") "")
(check-expect (string-remove-last "") "")

(define (string-remove-last str)
  (if (= (string-length str) 0) str
      (substring str 0 (sub1 (string-length str)))))

; string-first: String -> String
; Extracts the first character from string str.

(check-expect (string-first "h") "h")
(check-expect (string-first "hello") "h")
(check-expect (string-first "") "")

(define (string-first str)
  (if (= (string-length str) 0) str
      (string-ith str 0)))

; string-last: String -> String
; Extracts the last character from string str/.

(check-expect (string-last "h") "h")
(check-expect (string-last "hello") "o")
(check-expect (string-last "") "")

(define (string-last str)
  (if (= (string-length str) 0) str
      (string-ith str (sub1 (string-length str)))))

; string-rest: String -> String
; Produces a string like STR with the first character removed.

(check-expect (string-rest "") "")
(check-expect (string-rest "h") "")
(check-expect (string-rest "hello") "ello")

(define (string-rest str)
  (if (= (string-length str) 0) str
      (substring str 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions for handling key events
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; cursor-left: Editor -> Editor
; Moves cursor to the left in editor ed.

(check-expect (cursor-left (make-editor "abc" "def"))
              (make-editor "ab" "cdef"))

(check-expect (cursor-left (make-editor "" "abcdef"))
              (make-editor "" "abcdef"))

(define (cursor-left ed)
  (make-editor
   (string-remove-last (editor-pre ed))
   (string-append (string-last (editor-pre ed))
                  (editor-post ed))))

; cursor-right: Editor -> Editor
; Moves cursor to the right in editor ed.

(check-expect (cursor-right (make-editor "abc" "def"))
              (make-editor "abcd" "ef"))

(check-expect (cursor-right (make-editor "" "abcdef"))
              (make-editor "a" "bcdef"))

(check-expect (cursor-right (make-editor "abcdef" ""))
              (make-editor "abcdef" ""))

(define (cursor-right ed)
  (make-editor
   (string-append (editor-pre ed) (string-first (editor-post ed)))
   (string-rest (editor-post ed))))

; delete-left: Editor -> Editor
; Deletes 1 character to the left of the cursor in editor ed.

(check-expect (delete-left (make-editor "abc" "def"))
              (make-editor "ab" "def"))

(define (delete-left ed)
  (make-editor
   (string-remove-last (editor-pre ed))
   (editor-post ed)))

; insert-char: Editor -> Editor
; Inserts a char ke before cursor in editor ed.

(check-expect (insert-char (make-editor "hello " "world") "x")
              (make-editor "hello x" "world"))

(define (insert-char ed ke)
  (make-editor
   (string-append (editor-pre ed) ke)
   (editor-post ed)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Editing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; edit: Editor KeyEvent -> Editor
; Adds ke to the end of the pre field of ed, unless ke denotes
; the backspace ("\b") key. In that case, it deletes the character
; immediately to the left of the cursor (if there are any).
; The function ignores the tab key ("\t") and the return key ("\r").

(check-expect (edit ED1 "left")
              (make-editor "hello" " world"))

(check-expect (edit ED1 "right")
              (make-editor "hello w" "orld"))

(check-expect (edit ED1 "\b")
              (make-editor "hello" "world"))

(check-expect (edit ED1 "\t")
              (make-editor "hello " "world"))

(check-expect (edit ED1 "\r")
              (make-editor "hello " "world"))

(check-expect (edit ED1 "x")
              (make-editor "hello x" "world"))

(check-expect (edit ED1 "*")
              (make-editor "hello *" "world"))

(define (edit ed ke)
  (cond
    [(key=? ke "\b") (delete-left ed)]
    [(or (key=? ke "\r")
         (key=? ke "\t")) ed]
    [(equal? (string-length ke) 1) (insert-char ed ke)]
    [(key=? ke "left") (cursor-left ed)]
    [(key=? ke "right") (cursor-right ed)]
    [else ed]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Running the editor
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; run: Editor -> Editor
; Launches the editor with initial state ed.

(define (run ed)
  (big-bang ed
            [to-draw render]
            [on-key edit]))