;; defjs - Common Lisp REPL fun in Javascript
;; Copyright (C) 2014 Matthew Carter
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;;; defjs.asd

(asdf:defsystem #:defjs
  :serial t
  :description "Common Lisp REPL fun in Javascript"
  :author "Matthew Carter <m@ahungry.com>"
  :license "GPLv3"
  :depends-on (#:bordeaux-threads
               #:parenscript
               #:clws
               #:hunchentoot
               #:cl-who
               #:cl-json
               #:glyphs)
  :components ((:file "package")
               (:file "defjs")))
