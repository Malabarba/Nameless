#+OPTIONS: toc:nil num:nil

* Nameless --- /less is more/
*Hide package namespaces in your emacs-lisp code.*

Simply put, turn on this minor mode, and the namespace prefix of the
package you’re editing will be hidden by a ~:~. Here’s a comparison.
The image to the *left* is what you normally see. The image to
the *right* has ~nameless-mode~ turned on.\\
 [[file:example-nameless.png]]

** Usage

To use this package add the following configuration to your Emacs init file.

#+BEGIN_SRC emacs-lisp
(add-hook 'emacs-lisp-mode-hook #'nameless-mode)
#+END_SRC

You can configure a string to use instead of ~:~ by setting the
~nameless-prefix~, and the name of the face used is ~nameless-face~.
You can even just hide the prefix completely by setting this variable
to an empty string.

While the mode is active, the =C-c C--= key inserts the
package namespace if appropriate.

* Configuration

** Quickly typing the namespace
~nameless-mode~ binds the =C-c C--= key to
~nameless-insert-name~, which immediately inserts the current name for
you, or even expands aliases to the names they point to.

Let’s say you’re in a file called ~foo-bar.el~.
#+BEGIN_SRC text
   C-c C-- → foo-bar-
fl C-c C-- → font-lock-
#+END_SRC

There’s also a command called ~nameless-insert-name-or-self-insert~.
You can bind this to the =_= key and make it even faster to
insert the name.
** Configuring the namespace name
Nameless guesses the package name with the ~lm-get-package-name~
function, but sometimes this might not match the name you want to use.

In these situations, simply set ~nameless-current-name~ as file-local variable.
To do that, invoke the following command:
#+BEGIN_SRC text
M-x add-file-local-variable RET nameless-current-name RET "package-name"
#+END_SRC
You can also set the same name for all lisp files in a project by
setting dir-local variables with ~M-x add-file-local-variable~.

If you /don’t/ want Nameless to use a namespace name at all (neither
manual nor automatic), you can set ~nameless-discover-current-name~ to
~nil~. This will disable this functionality, so that Nameless will
/only/ use aliases (see next item).

** Requiring other packages as aliases
Nameless can also be used to “import” other packages as aliases. For
instance, in the default behaviour, functions in the ~font-lock~
package (e.g., ~font-lock-add-keywords~) will be displayed with the
~fl:~ prefix (e.g., ~fl:add-keywords~).

You can configure your own aliases globally with ~nameless-global-aliases~.
#+BEGIN_SRC emacs-lisp
(setq nameless-global-aliases '(("fl" . "font-lock")
                                ("s" . "seq")
                                ("me" . "macroexp")
                                ("c" . "cider")
                                ("q" . "queue")))
#+END_SRC

You can also configure aliases per-file by setting ~nameless-aliases~
as a file-local variable.
#+BEGIN_SRC emacs-lisp
;; Local Variables:
;; nameless-aliases: (("c" . "cider"))
;; End:
#+END_SRC
Note that there’s no ~quote~ before ~((c~!\\
You can also configure it for a whole project, by setting it as a dir-local variable.

** Private symbols

Private symbols in elisp are written with an extra dash after the
prefix (e.g., ~foobar--indent-impl~). With Nameless, these are usually
displayed as ~:-indent-impl~, but you can also make them be displayed
as ~::indent-impl~ by setting

#+BEGIN_SRC emacs-lisp
(setq nameless-private-prefix t)
#+END_SRC

** Packages that don’t use ~-~ (hyphen) as a separator
You can set ~nameless-separator~ file-locally to whatever separator
you package uses. Most packages use hyphens, by some use ~/~, ~|~, or
~:~.

You can also set it to ~nil~ globally and the separator will never be
hidden.
** Indentation and paragraph filling
Hiding parts of symbols could affect the way Emacs indents your code
and fills your paragraphs. Nameless lets you decide whether you want
that to happen or not.

The default behavior is that code is indented according to what you
see (i.e., according to short symbols), but text inside strings is
*not*. So text inside strings will be filled in the same way as if you
didn’t have ~nameless-mode~. Here’s how a docstring might be filled
with ~nameless-mode~ enabled:
#+BEGIN_SRC text
If point is immediately after an alias configured in the name you
had in `:aliases' or `:global-aliases', replace
it with the full name for that alias.
#+END_SRC
Altough it may look strange that the second line is so short, that’s
the correct way. When view on a ~*Help*~ buffer, that docstring will
look like this:
#+BEGIN_SRC text
If point is immediately after an alias configured in the name you
had in `nameless-aliases' or `nameless-global-aliases', replace
it with the full name for that alias.
#+END_SRC

To change this behavior, configure the variable
~nameless-affect-indentation-and-filling~.
