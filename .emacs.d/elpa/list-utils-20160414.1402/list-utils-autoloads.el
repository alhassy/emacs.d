;;; list-utils-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "list-utils" "list-utils.el" (0 0 0 0))
;;; Generated autoloads from list-utils.el

(let ((loads (get 'list-utils 'custom-loads))) (if (member '"list-utils" loads) nil (put 'list-utils 'custom-loads (cons '"list-utils" loads))))

(require 'cl)

(defstruct tconc head tail)

(autoload 'tconc-list "list-utils" "\
Efficiently append LIST to TC.

TC is a data structure created by `make-tconc'.

\(fn TC LIST)" nil nil)

(autoload 'tconc "list-utils" "\
Efficiently append ARGS to TC.

TC is a data structure created by `make-tconc'

Without ARGS, return the list held by TC.

\(fn TC &rest ARGS)" nil nil)

(autoload 'list-utils-cons-cell-p "list-utils" "\
Return non-nil if CELL holds a cons cell rather than a proper list.

A proper list is defined as a series of cons cells in which the
cdr slot of each cons holds a pointer to the next element of the
list, and the cdr slot in the final cons holds nil.

A plain cons cell, for the purpose of this function, is a single
cons in which the cdr holds data rather than a pointer to the
next cons cell, eg

    '(1 . 2)

In addition, a list which is not nil-terminated is not a proper
list and will be recognized by this function as a cons cell.
Such a list is printed using dot notation for the last two
elements, eg

    '(1 2 3 4 . 5)

Such improper lists are produced by `list*'.

\(fn CELL)" nil nil)

(autoload 'list-utils-make-proper-copy "list-utils" "\
Copy a cons cell or improper LIST into a proper list.

If optional TREE is non-nil, traverse LIST, making proper
copies of any improper lists contained within.

Optional RECUR-INTERNAL is for internal use only.

Improper lists consist of proper lists consed to a final
element, and are produced by `list*'.

\(fn LIST &optional TREE RECUR-INTERNAL)" nil nil)

(autoload 'list-utils-make-proper-inplace "list-utils" "\
Make a cons cell or improper LIST into a proper list.

Improper lists consist of proper lists consed to a final
element, and are produced by `list*'.

If optional TREE is non-nil, traverse LIST, making any
improper lists contained within into proper lists.

Optional RECUR-INTERNAL is for internal use only.

Modifies LIST and returns the modified value.

\(fn LIST &optional TREE RECUR-INTERNAL)" nil nil)

(autoload 'list-utils-make-improper-copy "list-utils" "\
Copy a proper LIST into an improper list.

Improper lists consist of proper lists consed to a final
element, and are produced by `list*'.

If optional TREE is non-nil, traverse LIST, making proper
copies of any improper lists contained within.

Optional RECUR-INTERNAL is for internal use only.

\(fn LIST &optional TREE RECUR-INTERNAL)" nil nil)

(autoload 'list-utils-make-improper-inplace "list-utils" "\
Make proper LIST into an improper list.

Improper lists consist of proper lists consed to a final
element, and are produced by `list*'.

If optional TREE is non-nil, traverse LIST, making any
proper lists contained within into improper lists.

Optional RECUR-INTERNAL is for internal use only.

Modifies LIST and returns the modified value.

\(fn LIST &optional TREE RECUR-INTERNAL)" nil nil)

(autoload 'list-utils-linear-subseq "list-utils" "\
Return the linear elements from a partially cyclic LIST.

If there is no cycle in LIST, return LIST.  If all elements of
LIST are included in a cycle, return nil.

As an optimization, CYCLE-LENGTH may be specified if the length
of the cyclic portion is already known.  Otherwise it will be
calculated from LIST.

\(fn LIST &optional CYCLE-LENGTH)" nil nil)

(autoload 'list-utils-cyclic-subseq "list-utils" "\
Return any cyclic elements from LIST as a circular list.

The first element of the cyclic structure is not guaranteed to be
first element of the return value unless FROM-START is non-nil.

To linearize the return value, use `list-utils-make-linear-inplace'.

If there is no cycle in LIST, return nil.

\(fn LIST &optional FROM-START)" nil nil)

(autoload 'list-utils-cyclic-length "list-utils" "\
Return the number of cyclic elements in LIST.

If some portion of LIST is linear, only the cyclic
elements will be counted.

If LIST is completely linear, return 0.

\(fn LIST)" nil nil)

(autoload 'list-utils-cyclic-p "list-utils" "\
Return non-nil if LIST contains any cyclic structures.

If optional PERFECT is set, only return non-nil if LIST is a
perfect non-branching cycle in which the last element points
to the first.

\(fn LIST &optional PERFECT)" nil nil)

(autoload 'list-utils-linear-p "list-utils" "\
Return non-nil if LIST is linear (no cyclic structure).

\(fn LIST)" nil nil)

(defalias 'list-utils-improper-p 'list-utils-cons-cell-p)

(autoload 'list-utils-safe-length "list-utils" "\
Return the number of elements in LIST.

LIST may be linear or cyclic.

If LIST is not really a list, returns 0.

If LIST is an improper list, return the number of proper list
elements, like `safe-length'.

\(fn LIST)" nil nil)

(autoload 'list-utils-flat-length "list-utils" "\
Count simple elements from the beginning of LIST.

Stop counting when a cons is reached.  nil is not a cons,
and is considered to be a \"simple\" element.

If the car of LIST is a cons, return 0.

\(fn LIST)" nil nil)

(autoload 'list-utils-make-linear-copy "list-utils" "\
Return a linearized copy of LIST, which may be cyclic.

If optional TREE is non-nil, traverse LIST, substituting
linearized copies of any cyclic lists contained within.

\(fn LIST &optional TREE)" nil nil)

(autoload 'list-utils-make-linear-inplace "list-utils" "\
Linearize LIST, which may be cyclic.

Modifies LIST and returns the modified value.

If optional TREE is non-nil, traverse LIST, linearizing any
cyclic lists contained within.

\(fn LIST &optional TREE)" nil nil)

(autoload 'list-utils-safe-equal "list-utils" "\
Compare LIST-1 and LIST-2, which may be cyclic lists.

LIST-1 and LIST-2 may also contain cyclic lists, which are
each traversed and compared.  This function will not infloop
when cyclic lists are encountered.

Non-nil is returned only if the leaves of LIST-1 and LIST-2 are
`equal' and the structure is identical.

Optional TEST specifies a test, defaulting to `equal'.

If LIST-1 and LIST-2 are not actually lists, they are still
compared according to TEST.

\(fn LIST-1 LIST-2 &optional TEST)" nil nil)

(autoload 'list-utils-depth "list-utils" "\
Find the depth of LIST, which may contain other lists.

If LIST is not a list or is an empty list, returns a depth
of 0.

If LIST is a cons cell or a list which does not contain other
lists, returns a depth of 1.

\(fn LIST)" nil nil)

(autoload 'list-utils-flatten "list-utils" "\
Return a flattened copy of LIST, which may contain other lists.

This function flattens cons cells as lists, and
flattens circular list structures.

\(fn LIST)" nil nil)

(autoload 'list-utils-insert-before "list-utils" "\
Look in LIST for ELEMENT and insert NEW-ELEMENT before it.

Optional TEST sets the test used for a matching element, and
defaults to `equal'.

LIST is modified and the new value is returned.

\(fn LIST ELEMENT NEW-ELEMENT &optional TEST)" nil nil)

(autoload 'list-utils-insert-after "list-utils" "\
Look in LIST for ELEMENT and insert NEW-ELEMENT after it.

Optional TEST sets the test used for a matching element, and
defaults to `equal'.

LIST is modified and the new value is returned.

\(fn LIST ELEMENT NEW-ELEMENT &optional TEST)" nil nil)

(autoload 'list-utils-insert-before-pos "list-utils" "\
Look in LIST for position POS, and insert NEW-ELEMENT before.

POS is zero-indexed.

LIST is modified and the new value is returned.

\(fn LIST POS NEW-ELEMENT)" nil nil)

(autoload 'list-utils-insert-after-pos "list-utils" "\
Look in LIST for position POS, and insert NEW-ELEMENT after.

LIST is modified and the new value is returned.

\(fn LIST POS NEW-ELEMENT)" nil nil)

(autoload 'list-utils-and "list-utils" "\
Return the elements of LIST1 which are present in LIST2.

This is similar to `cl-intersection' (or `intersection') from
the cl library, except that `list-utils-and' preserves order,
does not uniquify the results, and exhibits more predictable
performance for large lists.

Order will follow LIST1.  Duplicates may be present in the result
as in LIST1.

TEST is an optional comparison function in the form of a
hash-table-test.  The default is `equal'.  Other valid values
include `eq' (built-in), `eql' (built-in), `list-utils-htt-='
\(numeric), `list-utils-htt-case-fold-equal' (case-insensitive).
See `define-hash-table-test' to define your own tests.

HINT is an optional micro-optimization, predicting the size of
the list to be hashed (LIST2 unless FLIP is set).

When optional FLIP is set, the sense of the comparison is
reversed.  When FLIP is set, LIST2 will be the guide for the
order of the result, and will determine whether duplicates may
be returned.  Since this function preserves duplicates, setting
FLIP can change the number of elements in the result.

Performance: `list-utils-and' and friends use a general-purpose
hashing approach.  `intersection' and friends use pure iteration.
Iteration can be much faster in a few special cases, especially
when the number of elements is small.  In other scenarios,
iteration can be much slower.  Hashing has no worst-case
performance scenario, although it uses much more memory.  For
heavy-duty list operations, performance may be improved by
`let'ing `gc-cons-threshold' to a high value around sections that
make frequent use of this function.

\(fn LIST1 LIST2 &optional TEST HINT FLIP)" nil nil)

(autoload 'list-utils-not "list-utils" "\
Return the elements of LIST1 which are not present in LIST2.

This is similar to `cl-set-difference' (or `set-difference') from
the cl library, except that `list-utils-not' preserves order and
exhibits more predictable performance for large lists.  Order will
follow LIST1.  Duplicates may be present as in LIST1.

TEST is an optional comparison function in the form of a
hash-table-test.  The default is `equal'.  Other valid values
include `eq' (built-in), `eql' (built-in), `list-utils-htt-='
\(numeric), `list-utils-htt-case-fold-equal' (case-insensitive).
See `define-hash-table-test' to define your own tests.

HINT is an optional micro-optimization, predicting the size of
the list to be hashed (LIST2 unless FLIP is set).

When optional FLIP is set, the sense of the comparison is
reversed, returning elements of LIST2 which are not present in
LIST1.  When FLIP is set, LIST2 will be the guide for the order
of the result, and will determine whether duplicates may be
returned.

Performance: see notes under `list-utils-and'.

\(fn LIST1 LIST2 &optional TEST HINT FLIP)" nil nil)

(autoload 'list-utils-xor "list-utils" "\
Return elements which are only present in either LIST1 or LIST2.

This is similar to `cl-set-exclusive-or' (or `set-exclusive-or')
from the cl library, except that `list-utils-xor' preserves order,
and exhibits more predictable performance for large lists.  Order
will follow LIST1, then LIST2.  Duplicates may be present as in
LIST1 or LIST2.

TEST is an optional comparison function in the form of a
hash-table-test.  The default is `equal'.  Other valid values
include `eq' (built-in), `eql' (built-in), `list-utils-htt-='
\(numeric), `list-utils-htt-case-fold-equal' (case-insensitive).
See `define-hash-table-test' to define your own tests.

HINT is an optional micro-optimization, predicting the size of
the list to be hashed (LIST2 unless FLIP is set).

When optional FLIP is set, the sense of the comparison is
reversed, causing order and duplicates to follow LIST2, then
LIST1.

Performance: see notes under `list-utils-and'.

\(fn LIST1 LIST2 &optional TEST HINT FLIP)" nil nil)

(autoload 'list-utils-uniq "list-utils" "\
Return a uniquified copy of LIST, preserving order.

This is similar to `cl-remove-duplicates' (or `remove-duplicates')
from the cl library, except that `list-utils-uniq' preserves order,
and exhibits more predictable performance for large lists.  Order
will follow LIST.

TEST is an optional comparison function in the form of a
hash-table-test.  The default is `equal'.  Other valid values
include `eq' (built-in), `eql' (built-in), `list-utils-htt-='
\(numeric), `list-utils-htt-case-fold-equal' (case-insensitive).
See `define-hash-table-test' to define your own tests.

HINT is an optional micro-optimization, predicting the size of
LIST.

Performance: see notes under `list-utils-and'.

\(fn LIST &optional TEST HINT)" nil nil)

(autoload 'list-utils-dupes "list-utils" "\
Return only duplicated elements from LIST, preserving order.

Duplicated elements may still exist in the result: this function
removes singlets.

TEST is an optional comparison function in the form of a
hash-table-test.  The default is `equal'.  Other valid values
include `eq' (built-in), `eql' (built-in), `list-utils-htt-='
\(numeric), `list-utils-htt-case-fold-equal' (case-insensitive).
See `define-hash-table-test' to define your own tests.

HINT is an optional micro-optimization, predicting the size of
LIST.

Performance: see notes under `list-utils-and'.

\(fn LIST &optional TEST HINT)" nil nil)

(autoload 'list-utils-singlets "list-utils" "\
Return only singlet elements from LIST, preserving order.

Duplicated elements may not exist in the result.

TEST is an optional comparison function in the form of a
hash-table-test.  The default is `equal'.  Other valid values
include `eq' (built-in), `eql' (built-in), `list-utils-htt-='
\(numeric), `list-utils-htt-case-fold-equal' (case-insensitive).
See `define-hash-table-test' to define your own tests.

HINT is an optional micro-optimization, predicting the size of
LIST.

Performance: see notes under `list-utils-and'.

\(fn LIST &optional TEST HINT)" nil nil)

(autoload 'list-utils-partition-dupes "list-utils" "\
Partition LIST into duplicates and singlets, preserving order.

The return value is an alist with two keys: 'dupes and 'singlets.
The two values of the alist are lists which, if combined, comprise
a complete copy of the elements of LIST.

Duplicated elements may still exist in the 'dupes partition.

TEST is an optional comparison function in the form of a
hash-table-test.  The default is `equal'.  Other valid values
include `eq' (built-in), `eql' (built-in), `list-utils-htt-='
\(numeric), `list-utils-htt-case-fold-equal' (case-insensitive).
See `define-hash-table-test' to define your own tests.

HINT is an optional micro-optimization, predicting the size of
LIST.

Performance: see notes under `list-utils-and'.

\(fn LIST &optional TEST HINT)" nil nil)

(autoload 'list-utils-alist-or-flat-length "list-utils" "\
Count simple or cons-cell elements from the beginning of LIST.

Stop counting when a proper list of non-zero length is reached.

If the car of LIST is a list, return 0.

\(fn LIST)" nil nil)

(autoload 'list-utils-alist-flatten "list-utils" "\
Flatten LIST, which may contain other lists.  Do not flatten cons cells.

It is not guaranteed that the result contains *only* cons cells.
The result could contain other data types present in LIST.

This function simply avoids flattening single conses or improper
lists where the last two elements would be expressed as a dotted
pair.

\(fn LIST)" nil nil)

(autoload 'list-utils-plist-reverse "list-utils" "\
Return reversed copy of property-list PLIST, maintaining pair associations.

\(fn PLIST)" nil nil)

(autoload 'list-utils-plist-del "list-utils" "\
Delete from PLIST the property PROP and its associated value.

When PROP is not present in PLIST, there is no effect.

The new plist is returned; use `(setq x (list-utils-plist-del x prop))'
to be sure to use the new value.

This functionality overlaps with the undocumented `cl-do-remf'.

\(fn PLIST PROP)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "list-utils" '("list-utils-htt-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; list-utils-autoloads.el ends here
