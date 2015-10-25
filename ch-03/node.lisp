(defun find-all (item seq &rest keyword-args &key (test #'eql) test-not &allow-other-keys)
  (if test-not
      (apply #'remove item seq
             :test-not (complement test-not) keyword-args)
      (apply #'remove item seq
             :test (complement test) keyword-args)))

(defvar *state* nil "The current state: a list of conditions.")

(defvar *ops* nil "A list of available operators.")

(defstruct op "An operation"
  (action nil) (preconds nil) (add-list nil) (del-list nil))


(defparameter *skate*
  (list
   (make-op
    :action 'go-skate
    :preconds '(have-skate energy)
    :add-list '(have-fun)
    :del-list '(energy))
   (make-op
    :action 'buy-board
    :preconds '(have-money)
    :add-list '(have-board)
    :del-list '(have-money))
   (make-op
    :action 'got-energy
    :preconds '(redbull monster have-money-2$)
    :add-list '(energy)
    :del-list '(redbull monster have-money-2$))))




(defun GPS (*state* goals *ops*)
  "General Problem Solver: achieve all goals using *ops*."
  (if (achieve-all goals) 'solved))


(defun achieve (goal)
  "A goal is achieved if it already holds,
  or if there is an appropriate op for it that is applicable."
  (or (member goal *state*)
      (some #'apply-op 
            (find-all goal *ops* :test #'appropriate-p))))

(defun appropriate-p (goal op)
  "An op is appropriate to a goal if it is in its add list."
  (member goal (op-add-list op)))

(defun apply-op (op)
  "Print a message and update *state* if op is applicable."
  (when (every #'achieve (op-preconds op))
    (print (list 'executing (op-action op)))
    (setf *state* (set-difference *state* (op-del-list op)))
    (setf *state* (union *state* (op-add-list op)))
    t))

(defun achieve-all (goals)
  (and (every #'achieve goals) (subsetp goals *state*)))

