# mark-select.el: An Emacs library for mark and select interface
## Overview
mark-select.el is an Emacs library to mark and select from specified multiple options like the interface used in dired.
## How to use
This library can be used like below code.
<pre><code>(let ((hash (make-hash-table :test 'equal)))
  (setf (gethash "lang" hash) '("Select program languages"
                                (("Lisp" . t) "Perl" "Ruby")))
  (setf (gethash "file" hash) '("Select files"
                                (("file1" . t)
                                 ("file2" . t)
                                 ("file3" . t))))
  (mark-select-multi hash))
</pre></code>