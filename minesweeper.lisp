;;; minesweeper.lisp --- Minesweeper

;; Author: Peter Hua
;; Version: $Revision: 1.0$
;; Keywords: $keywords

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING. If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; (minesweeper-cli)
;; (minesweeper-server)
;; (minesweeper-ncurses)

;;; Code:

(ql:quickload '("portable-threads" "usocket"))
(load "~/quicklisp/dists/quicklisp/software/usocket-0.7.1/server.lisp")

(in-package #:common-lisp-user)

(defpackage #:lisp.games.minesweeper
  (:use #:common-lisp
        #:portable-threads
        #:usocket)
  (:export #:minesweeper-cli
           #:minesweeper-server))

(in-package #:lisp.games.minesweeper)

(defstruct cell
  "A minefield cell."
  (mine-count 0 :type integer)
  (mine-p nil :type boolean)
  (flag-p nil :type boolean)
  (visible-p nil :type boolean)
  (neighbors nil :type list))

(let ((neighborhood '((-1 -1) (-1 0) (-1 1) (0 -1) (0 1) (1 -1) (1 0) (1 1))))
  (defun get-neighbors (row column minefield)
    "Get neighbors at (`row', `column').
=> a list of subscripts."
    (remove-if-not #'(lambda (neighbor) (array-in-bounds-p minefield (car neighbor) (cadr neighbor)))
                   (mapcar #'(lambda (neighbor) (mapcar #'+ (list row column) neighbor)) neighborhood))))

(let ((min-dimensions '(3 3))
      (max-dimensions '(30 24))
      (max-mine-count 0.92638))
  (defun make-minefield (&optional (dimensions '(30 24)) (mine-count 667))
    "Make minefield and set mines.
=> `minefield'."
    (let* ((dimensions (mapcar #'min (mapcar #'max dimensions min-dimensions) max-dimensions))
           (mine-count (min mine-count (floor (* (apply #'* dimensions) max-mine-count))))
           (minefield (make-array dimensions :element-type 'cell))
           (size (array-total-size minefield)))
      (dotimes (row (car dimensions))
        (dotimes (column (cadr dimensions))
          (setf (aref minefield row column) (make-cell))
          (setf (cell-neighbors (aref minefield row column)) (get-neighbors row column minefield))))
      (do ((index (random size) (random size)))
          ((<= mine-count 0) minefield)
        (let ((cell (row-major-aref minefield index)))
          (unless (cell-mine-p cell)
            (setf (cell-mine-p cell) t)
            (dolist (neighbor (cell-neighbors cell))
              (incf (cell-mine-count (aref minefield (car neighbor) (cadr neighbor)))))
            (setf mine-count (1- mine-count))))))))

;; Commands

(defmacro with-cell (row column minefield &body body)
  "Anaphoric macro that performs array bounds checking and binds symbol `cell'."
  `(when (array-in-bounds-p ,minefield ,row ,column)
     (let ((cell (aref ,minefield ,row ,column)))
       ,@body)))

(defun clear-minefield (row column minefield &optional (results nil))
  "Clear minefield recursively.
=> `results', a list of subscripts modified and their values."
  (with-cell row column minefield
    (unless (cell-visible-p cell)
      (setf (cell-visible-p cell) t)
      (push (list row column (cell-value cell)) results)
      (unless (or (/= (cell-mine-count cell) 0) (cell-flag-p cell) (cell-mine-p cell))
        (dolist (neighbor (cell-neighbors cell) results)
          (setf results (clear-minefield (car neighbor) (cadr neighbor) minefield results))))))
  results)

(defun toggle-flag (row column minefield)
  "Toggle flag at (`row', `column').
=> a list of subscripts modified and their values."
  (with-cell row column minefield
    (setf (cell-flag-p cell) (not (cell-flag-p cell)))
    (list (list row column (cell-value cell)))))

(defun reset-minefield (minefield)
  "Reset minefield and unset flags.
=> `minefield'."
  (dotimes (index (array-total-size minefield) minefield)
    (let ((cell (row-major-aref minefield index)))
      (setf (cell-flag-p cell) nil)
      (setf (cell-visible-p cell) nil))))

(defun end-game-p (command row column minefield)
  "Check for end of game. A game is won iff all mines are correctly flagged.
=> `WIN' or `LOSE' if end of game; otherwise, `nil'."
  (with-cell row column minefield
    (if (and (eq command 'clear) (cell-mine-p cell))
        'LOSE
        (dotimes (index (array-total-size minefield) 'WIN)
          (let ((cell (row-major-aref minefield index)))
            (unless (eq (cell-mine-p cell) (cell-flag-p cell))
              (return-from end-game-p nil)))))))

(defun reveal-minefield (minefield)
  "Reveal minefield.
=> `minefield'."
  (dotimes (index (array-total-size minefield) minefield)
    (setf (cell-visible-p (row-major-aref minefield index)) t)))

(defun save-minefield (minefield filename)
  "Save minefield to filename."
  (with-open-file (stream filename :direction :output :if-exists :supersede)
    (princ minefield stream)))

(defun load-minefield (filename)
  "Load minefield from filename.
=> `minefield'."
  (with-open-file (stream filename :direction :input :if-does-not-exist :error)
    (read stream)))

;; Command Line Interface

(defun cell-value (cell)
  "Convert cell to a printable value."
  (cond ((cell-flag-p cell) #\?)
        ((not (cell-visible-p cell)) #\.)
        ((cell-mine-p cell) #\!)
        ((> (cell-mine-count cell) 0) (cell-mine-count cell))
        (t #\SPACE)))

(defun print-minefield (minefield &optional (stream t))
  "Print minefield to stream."
  (let* ((dimensions (array-dimensions minefield))
         (rows (first dimensions))
         (columns (second dimensions)))
    (flet ((print-columns ()
             (format stream "    ")
             (dotimes (c columns) (format stream "~4D" c)))
           (print-separator ()
             (print-newline :stream stream)
             (format stream "    +")
             (loop repeat columns do (format stream "---+"))
             (print-newline :stream stream)))
      (print-columns)
      (print-separator)
      (dotimes (r rows)
        (format stream "~3D |" r)
        (dotimes (c columns) (format stream " ~A |" (cell-value (aref minefield r c))))
        (print-separator))
      (print-columns))))

(defun print-commands (&optional (stream t))
  "Print commands to stream."
  (format stream "~
COMMANDS
  clear ROW COLUMN    Clear cell at ROW COLUMN.
  flag  ROW COLUMN    Toggle flag at ROW COLUMN.
  restart             Reset minefield.
  save FILENAME       Save minefield to FILENAME.
  load FILENAME       Load minefield from FILENAME.
  help                Print commands.
  quit                Quit.
"))

(defun print-help (&optional (stream t))
  "Print help to stream."
  (format stream "~
NAME
  Minesweeper.
OBJECTIVE
  Clear the minefield.
")
  (print-commands stream))

(defun print-newline (&key (count 1) (stream t))
  (loop repeat count do (terpri stream)))

(defun minesweeper-loop (&key (dimensions '(6 4)) (mine-count 5) (ostream t) (istream nil))
  "Minesweeper game loop."
  (let ((minefield (make-minefield dimensions mine-count)))
    (loop
       (print-minefield minefield ostream)
       (print-newline :count 2 :stream ostream)
       (format ostream "> ")
       (force-output ostream)
       ;; (let ((command (read-from-string (format nil "(~A)" (read-line istream nil)))))
       (let ((line (read-line istream nil)))
         (let ((command (read-from-string (format nil "(~A)" line))))
           (print-newline :stream ostream)
           (case (car command)
             (clear   (clear-minefield (cadr command) (caddr command) minefield))
             (flag    (toggle-flag (cadr command) (caddr command) minefield))
             (restart (reset-minefield minefield))
             (save    (save-minefield minefield (cadr command)))
             (load    (setf minefield (load-minefield (cadr command))))
             (help    (print-commands ostream) (print-newline :stream ostream))
             (quit    (return-from minesweeper-loop nil)))
           (let ((result (end-game-p (car command) (cadr command) (caddr command) minefield)))
             (when result
               (print-minefield (reveal-minefield minefield) ostream)
               (print-newline :count 2 :stream ostream)
               (format ostream "You ~A!" result)
               (force-output ostream)
               (return-from minesweeper-loop t))))))))

(defun minesweeper-cli (&optional (ostream t) (istream nil))
  "Minesweeper command line interface."
  (print-help ostream)
  (loop
     (print-newline :stream ostream)
     (format ostream "Enter minefield dimensions and mine count or press ENTER for defaults, ~
e.g. '6 4 5': ")
     (force-output ostream)
     ;; (let ((input (read-from-string (format nil "(~A)" (read-line istream nil)))))
     (let ((line (read-line istream nil)))
       (let ((input (read-from-string (format nil "(~A)" line))))
         (print-newline :stream ostream)
         (unless
             (case (length input)
               (3 (minesweeper-loop :dimensions (list (car input) (cadr input))
                                    :mine-count (caddr input)
                                    :ostream ostream :istream istream))
               (2 (minesweeper-loop :dimensions (list (car input) (cadr input))
                                    :ostream ostream :istream istream))
               (otherwise (minesweeper-loop :ostream ostream :istream istream)))
           (return-from minesweeper-cli)))
       (print-newline :count 2 :stream ostream)
       (format ostream "Enter 'Y' to play again: ")
       (force-output ostream)
       (unless (char-equal (read-char istream) #\Y)
         (return-from minesweeper-cli)))))

;; Socket Interface

(defun minesweeper-socket (stream)
  "Minesweeper socket interface."
  (minesweeper-cli stream stream))

(defun minesweeper-server (&key (host "127.0.0.1") (port 8080))
  "Minesweeper server."
  (socket-server host port #'minesweeper-socket nil :multi-threading nil))

;; ncurses Interface
