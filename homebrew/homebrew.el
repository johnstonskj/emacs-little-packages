;;; homebrew.el --- Homebrew package manager functions -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Simon Johnston

;; Author: Simon Johnston <johnstonskj@gmail.com>
;; Keywords: lisp
;; Version: 0.0.1
;; URL:

;; This file is not part of GNU Emacs.

;; MIT License

;; Copyright (c) 2023 Simon Johnston

;; Permission is hereby granted, free of charge, to any person obtaining a
;; copy of this software and associated documentation files (the "Software"),
;; to deal in the Software without restriction, including without limitation
;; the rights to use, copy, modify, merge, publish, distribute, sublicense,
;; and/or sell copies of the Software, and to permit persons to whom the
;; Software is furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;; DEALINGS IN THE SOFTWARE.

;;; Commentary:

;; This package provides a library interface to the Homebrew package manager
;; for Linux and macOS.

;;; Code:

;; ==========================================================================
;; Customization

(defgroup homebrew nil
  "Homebrew package manager.
This package provides a library interface to the Homebrew package manager for
Linux and macos. See URL `https://brew.sh/'."
  :prefix "homebrew-"
  :group 'processes)

(defconst homebrew--command-name "brew"
  "The name of the actual executable.")

(defconst homebrew--command-path
  (executable-find homebrew--command-name)
  "This is t if the brew command was found, else nil.")

(when (null homebrew--command-path)
  (display-warning
   '(homebrew)
   "Homebrew executable not found, try `homebrew-install-self'"
   :error
   nil))

(defcustom
  homebrew-command
  (or (executable-find homebrew--command-path) homebrew--command-name)
  "The executable name/path for the brew command.
This is usually in the \"bin\" directory within `homebrew-prefix'."
  :tag "Homebrew command name/path"
  :group 'homebrew
  :type 'string)

(defcustom
  homebrew-prefix
  (string-trim (shell-command-to-string
                (concat homebrew-command " --prefix")))
  "The prefix directory for the package manager.
This is the root for all Homebrew contents, specific directories can be
referenced by a set of functions named `homebrew-prefix/*'."
  :tag "Package manager prefix"
  :group 'homebrew
  :type 'directory)

;; ==========================================================================
;; Internal stuff

;;; Constants

(defconst homebrew-install--source
  "https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh"
  "The URL of the homebrew install script.")

(defconst homebrew--run-output-buffer "*Homebrew Output*"
  "The name of the buffer that captures output from the brew command.")

(defconst homebrew--common-flags-alist
  '((:debug "--debug")
    (:quiet "--quiet")
    (:verbose "--verbose"))
  "The common flags accepted by all brew commands.")

;;; Generic -- strings

(defun empty-string-p (string &optional no-trim)
  "Return non-nil if the STRING is empty or nil.
The value of NO-TRIM determines whether strings consisting of only spaces are
considered to be empty, by default STRING is passed to `string-trim'."
  (or (null string)
      (zerop (length (if no-trim
                         string
                       (string-trim string))))))

;;; Generic --- lists

(defun plist->alist (plist)
  "Convert PLIST, a property list, into an association list.
Each element in the resulting list is a two element list, not a dotted pair.
Elements where the `car' is not a keyword are removed."
  (seq-filter
   (lambda (pair) (keywordp (car pair)))
   (seq-partition plist 2)))

(defun alist-map (fn alist)
  "Map FN over the association list ALIST.
For each element in ALIST the function FN takes two arguments, the `car' and
`cdr' of the element."
  (mapcar (lambda (pair) (funcall fn (car pair) (cadr pair))) alist))

(defun plist-map (fn plist)
  "Map FN over the property list PLIST.
For each element in PLIST the function FN takes two arguments, the key and
value of the element. The element key is always a keyword, the value for flag
arguments should be either t or nil."
  (alist-map fn (plist->alist plist)))

;;; Generic --- messages

(defun wmessage (command message)
  "Write a formatted warning MESSAGE regarding the specific COMMAND."
  (message "Warning [homebrew]: `%s %s' %s"
           homebrew--command-name command message))

;;; Parsing arguments

(defun homebrew--parse-flags (args flags-alist)
  "Parse the property list ARGS using the flags defined in FLAGS-ALIST.
The flags association list maps keywords used within ARGS to string values
that are the actual command-line arguments used to call brew."
  (seq-filter
   (lambda (s) (not (empty-string-p s)))
   (plist-map
    (lambda (k v) (let ((flag (alist-get k flags-alist)))
               (if (and flag v) (car flag) "")))
    args)))

(defun homebrew--parse-name-list (names)
  "Parse NAMES into a list of strings.
The value of NAMES may be one of:

- a single string,
- a list of strings,
- a single symbol,
- a list of symbols,
- or a list of mixed symbols and strings.

Any symbol is converted to a string with `symbol-name', any empty strings are
removed from the resulting list."
  (let ((names (if (listp names) names (list names))))
    (seq-filter
     (lambda (s) (not (empty-string-p s)))
     (mapcar
      (lambda (name)
        (cond ((stringp name) name)
              ((symbolp name) (symbol-name name))
              (t "")))
      names))))

(defun homebrew--run (command args)
  "Run the brew tool with COMMAND and the list of arguments in ARGS.
The brew tool specified in `homebrew-command' is called with the actual
command in COMMAND and any addition arguments in ARGS."
  (let ((output-buffer (get-buffer-create homebrew--run-output-buffer)))
    (let ((result (apply
                   'call-process
                   homebrew-command
                   nil ;; infile
                   output-buffer
                   nil ;; display
                   (cons command args))))
      (if (and (integerp result) (= result 0))
          (progn
            (message "homebrew executed %s successfully" command)
            t)
        (progn
          (display-warning
           '(homebrew)
           (format
            "An error occured executing homebrew (%s)\ncmd: %s\nargs: %s"
            result
            homebrew-command
            (cons command args))
           :error
           nil)
          (set-buffer output-buffer)
          nil)))))

(defun homebrew--run-cmd (command args flags-alist)
  "Run a brew COMMAND that does not take formula/cask names.
The formula/cask NAMES are parsed with `homebrew--parse-name-list'.
The additional ARGS and FLAGS-ALIST are parsed with `homebrew--parse-flags'."
  (let ((common-args (homebrew--parse-flags
                      args
                      (append homebrew--common-flags-alist flags-alist))))
    (homebrew--run command common-args)))

(defun homebrew--run-package-cmd (command names args flags-alist)
  "Run a brew COMMAND that requires the formula/cask NAMES.
The formula/cask NAMES are parsed with `homebrew--parse-name-list'.
The additional ARGS and FLAGS-ALIST are parsed with `homebrew--parse-flags'."
  (let ((names (homebrew--parse-name-list names))
        (common-args (homebrew--parse-flags
                      args
                      (append homebrew--common-flags-alist flags-alist))))
    (if (null names)
        (wmessage command "resulted in empty name list")
      (homebrew--run command (append names common-args)))))

;; ==========================================================================
;; Public functions -- directories

(defun homebrew-prefix/package-prefix ()
  "The prefix directory for installed package links.
Links are created in this directory to the version-specific package installed
by homebrew. This can be a useful way to reference formulae or cask contents."
  (file-name-concat homebrew-prefix "opt"))

(defun homebrew-prefix/formulae ()
  "The location of downloaded formulae."
  (file-name-concat homebrew-prefix "Cellar"))

(defun homebrew-prefix/casks ()
  "The location of downloaded casks."
  (file-name-concat homebrew-prefix "Caskroom"))

(defun homebrew-prefix/bin ()
  "The location of links to installed binaries."
  (file-name-concat homebrew-prefix "bin"))

(defun homebrew-prefix/completions ()
  "The location of links to installed shell completions."
  (file-name-concat homebrew-prefix "completions"))

(defun homebrew-prefix/docs ()
  "The location of links to installed documentation."
  (file-name-concat homebrew-prefix "docs"))

(defun homebrew-prefix/etc ()
  "The location of links to installed configuration."
  (file-name-concat homebrew-prefix "etc"))

(defun homebrew-prefix/include ()
  "The location of links to installed header files."
  (file-name-concat homebrew-prefix "include"))

(defun homebrew-prefix/lib ()
  "The location of links to installed libraries."
  (file-name-concat homebrew-prefix "lib"))

(defun homebrew-prefix/manpages ()
  "The location of links to installed man pages."
  (file-name-concat homebrew-prefix "manpages"))

(defun homebrew-prefix/sbin ()
  "The location of links to installed static binaries."
  (file-name-concat homebrew-prefix "sbin"))

(defun homebrew-prefix/share ()
  "The location of links to share files."
  (file-name-concat homebrew-prefix "share"))

(defun homebrew-prefix/var ()
  "The location of temporary files."
  (file-name-concat homebrew-prefix "var"))

;; ==========================================================================
;; Public functions

(defun homebrew-install-self (&optional destination display)
  "Install the homebrew package manager itself.
The optional arguments are:

DESTINATION  The output destination as per `call-process'; the default
             value is the buffer named `homebrew--run-output-buffer'.
DISPLAY      The display as per `call-process'; the default value is t.

This is a bootstrap process and has to be done within a terminal as it may
require the execution of \"sudo\" and may ask questions."
  (let ((result (call-process
                 "/bin/bash"
                 nil ;; infile
                 (if (and destination (stringp destination))
                     destination
                   "*homebrew-install*") ;; destination buffer name
                 display
                 "-c"
                 (format "$(curl -fsSL %s)" homebrew-install--source))))
    (if (and (integerp result) (= result 0))
        t
      (progn
        (message "Could not install homebrew directly. Error: %s" result)
        nil))))

(defun homebrew-upgrade-self (&rest args)
  "Upgrade to the latest version of Homebrew and all formulae.

Fetch the newest version of Homebrew and all formulae from GitHub using
`git(1)' and perform any necessary migrations.

The ARGS plist may contain any of the following property flags where t turns
on the flag and nil turns off the flag.

:auto-update  Run on auto-updates (e.g. before brew install). Skips some
              slower steps.
:force        Always do a slower, full update check (even if unnecessary).
:merge        Use git merge to apply updates (rather than git rebase).

:debug        Display a trace of all shell commands as they are executed.
:quiet        Make some output more quiet.
:verbose      Print the directories checked and git operations performed."
  (homebrew--run-cmd "update" args '((:auto-update "--auto-update")
                                     (:force "--force")
                                     (:merge "--merge"))))

(defun homebrew-config (&rest args)
  "Show Homebrew and system configuration.

Show Homebrew and system configuration info useful for debugging. If you file
a bug report, you will be required to provide this information.

The ARGS plist may contain any of the following property flags where t turns
on the flag and nil turns off the flag.

:debug        Display a trace of all shell commands as they are executed.
:quiet        Make some output more quiet.
:verbose      Print the directories checked and git operations performed."
  (homebrew--run-cmd "config" args nil))

(defun homebrew-analytics (&optional setting &rest args)
  "Control Homebrew's anonymous aggregate user behaviour analytics.

The value of SETTING argument determines whether this function returns the
current state or turns the analytics on or off. Read more at
URL `https://docs.brew.sh/Analytics.'

Display the current state of Homebrew's analytics.

  (homebrew-analytics)

Turn Homebrew's analytics on or off respectively.

  (homebrew-analytics 'turn-on)
  (homebrew-analytics 'turn-off)

The ARGS plist may contain any of the following property flags where t turns
on the flag and nil turns off the flag.

:debug        Display a trace of all shell commands as they are executed.
:quiet        Make some output more quiet.
:verbose      Print the directories checked and git operations performed."
  (wmessage "analytics" "didn't parse args")
  (homebrew--run-package-cmd "analytics" names args '()))

(defun homebrew-doctor (&rest args)
  "Check your system for potential problems.

This function will return nil if any potential problems are found. Please
note that these warnings are just used to help the Homebrew maintainers with
debugging if you file an issue. If everything you use Homebrew for is working
fine: please don't worry or file an issue; just ignore this.

The ARGS plist may contain any of the following property flags where t turns
on the flag and nil turns off the flag.

:list-checks  List all audit methods, which can be run individually if
              provided as arguments.
:audit-debug  Enable debugging and profiling of audit methods.

:debug        Display a trace of all shell commands as they are executed.
:quiet        Make some output more quiet.
:verbose      Print the directories checked and git operations performed."
  (homebrew--run-cmd "doctor" args '((:audit-debug "--audit-debug")
                                     (:list-checks "--list-checks"))))

(defun homebrew-cleanup (&rest args)
  "Remove stale lock files and outdated downloads.

Remove stale lock files and outdated downloads for all formulae and casks, and
remove old versions of installed formulae. If arguments are specified, only do
this for the given formulae and casks. Removes all downloads more than 120
days old. This can be adjusted with `HOMEBREW_CLEANUP_MAX_AGE_DAYS'.

The ARGS plist may contain any of the following property flags where t turns
on the flag and nil turns off the flag.

:prune DAYS   Remove all cache files older than specified days. If you want
              to remove everything, use `all'.
:dry-run      Show what would be removed, but do not actually remove anything.
              Scrub the cache, including downloads for even the latest
              versions. Note that downloads for any installed formulae or
              casks will still not be deleted. If you want to delete those
              too:

                > rm -rf $(brew --cache)

:prune-prefix
              Only prune the symlinks and directories from the prefix and
              remove no other files.

:debug        Display a trace of all shell commands as they are executed.
:quiet        Make some output more quiet.
:verbose      Print the directories checked and git operations performed."
  (homebrew--run-cmd "cleanup" args '()))

(defun homebrew-outdated (&rest args)
  "List installed casks and formulae that have an updated version available.

By lt, version information is displayed in interactive shells, and
suped otherwise.

The ARGS plist may contain any of the following property flags where t turns
on the flag and nil turns off the flag.

:formula      List only outdated formulae.
:cask         List only outdated casks.
:json         Print output in JSON format. There are two versions: \"v1\" and
              \"v2\". \"v1\" is deprecated and is currently the default if no
              version is specified. \"v2\" prints outdated formulae and casks.
:fetch-HEAD   Fetch the upstream repository to detect if the HEAD installation
              of the formula is outdated. Otherwise, the repository's HEAD
              will only be checked for updates when a new stable or
              development version has been released.
:greedy       Also include outdated casks with \"auto_updates true\" or
              \"version :latest\".
:greedy-latest
              Also include outdated casks including those with \"version
              :latest\".
:greedy-auto-updates
              Also include outdated casks including those with \"auto_updates
              true\".

:debug        Display a trace of all shell commands as they are executed.
:quiet        Make some output more quiet.
:verbose      Print the directories checked and git operations performed."
  (homebrew--run-cmd "outdated" args '()))

(defun homebrew-autoremove (&rest args)
  "Uninstall formulae that are no longer needed.

Uninstall formulae that were only installed as a dependency of another formula
and are now no longer needed.

The ARGS plist may contain any of the following property flags where t turns
on the flag and nil turns off the flag.

:dry-run      List what would be uninstalled, but do not actually uninstall
              anything.

:debug        Display a trace of all shell commands as they are executed.
:quiet        Make some output more quiet.
:verbose      Print the directories checked and git operations performed."
  (homebrew--run-cmd "autoremove" args '((:dry-run "--dry-run"))))

(defun homebrew-list-installed (&rest args)
  "List all installed formulae and casks.

The ARGS plist may contain any of the following property flags where t turns
on the flag and nil turns off the flag.

:formula      List only formulae, or treat all named arguments as
              formulae.
:cask         List only casks, or treat all named arguments as casks.
:full-name    Print formulae with fully-qualified names. Unless
              `:full-name', `:versions' or `:pinned' are passed, other
              options (i.e. `:format-line', `:format-long', `:reverse',
              and `:by-time') are passed to `ls(1)' which produces the
              actual output.
:versions     Show the version number for installed formulae.
:multiple     Only show formulae with multiple versions installed.
:pinned       List only pinned formulae. See also pin, unpin.
:format-line  Force output to be one entry per line. This is the default
              when output is not to a terminal.
:format-long  List formulae and/or casks in long format.
:reverse      Reverse the order of the formulae and/or casks sort to list
              the oldest entries first.
:by-time      Sort formulae and/or casks by time modified, listing most
              recently modified first.

:debug        Display a trace of all shell commands as they are executed.
:quiet        Make some output more quiet.
:verbose      Print the directories checked and git operations performed."
  (let ((command "list"))
    (cond
     ((and (plist-get args :formula) (plist-get args :cask))
      (wmessage command
                "`:formula` and `:cask` are mutually exclusive"))
     ((and (plist-get args :cask)
           (or (plist-get args :multiple) (plist-get args :pinned)))
      (wmessage command
                "`:cask` does not allow `:multiple` or `:pinned`"))
     ((and (plist-get args :format-long)
           (or (plist-get args :full-name)
               (plist-get args :pinned)
               (plist-get args :versions)))
      (wmessage
       command
       "`:format-long` does not allow `:full-name`, `:pinned`, or `:versions`"))
     ((and (plist-get args :multiple) (not (plist-get args :versions)))
      (wmessage command
                "`:multiple` requires `:versions`"))
     (t (homebrew--run-cmd command args '((:formula "--formula")
                                          (:cask "--cask")
                                          (:full-name "--full-name")
                                          (:versions "--versions")
                                          (:multiple "--multiple")
                                          (:pinned "--pinned")
                                          (:format-line "-1")
                                          (:format-long "-l")
                                          (:reverse "-r")
                                          (:by-time "-t")))))))

(defun homebrew-package-search (text &rest args)
  "Perform a substring search of cask tokens and formula names for TEXT.

If text is flanked by slashes, it is interpreted as a regular expression.
The search for text is extended online to homebrew/core and homebrew/cask.

The ARGS plist may contain any of the following property flags where t turns
on the flag and nil turns off the flag.

:formula      Search online and locally for formulae.
:cask         Search online and locally for casks.
:desc         Search for formulae with a description matching text and casks
              with a name or description matching text.
:eval-all     Evaluate all available formulae and casks, whether installed or
              not, to search their descriptions. Implied if HOMEBREW_EVAL_ALL
              is set.
:pull-request
              Search for GitHub pull requests containing text.
:open         Search for only open GitHub pull requests.
:closed       Search for only closed GitHub pull requests.
:database     Search for text in the given databases. This value is a non-nil
              list containing a subset of the following symbols:

                '(repology macports opensuse fedora archlinux debian ubuntu)

:debug        Display a trace of all shell commands as they are executed.
:quiet        Make some output more quiet.
:verbose      Print the directories checked and git operations performed."
  nil)

(defun homebrew-package-info (names &rest args)
  "Display statistics for the Homebrew installation, or NAMES.

If a formula or cask is provided, show summary of information about it.
The ARGS plist may contain any of the following property flags where t turns
on the flag and nil turns off the flag.

:formula      Treat all named arguments as formulae.
:cask         Treat all named arguments as casks.
:analytics    List global Homebrew analytics data or, if specified,
              installation and build error data for formula (provided neither
              HOMEBREW_NO_ANALYTICS nor HOMEBREW_NO_GITHUB_API are set).
:days         How many days of analytics data to retrieve. The value for days
              must be 30, 90 or 365. The default is 30.
:category     Which type of analytics data to retrieve. The value for category
              must be install, install-on-request or build-error; cask-install
              or os-version may be specified if formula is not. The default is
              install.
:github       Open the GitHub source page for formula and cask in a browser.
              To view the history locally:

                > brew log -p formula or cask

:json         Print a JSON representation. Currently the default value for
              version is v1 for formula. For formula and cask use v2. See the
              docs for examples of using the JSON output:
              URL `https://docs.brew.sh/Querying-Brew'.
:installed    Print JSON of formulae that are currently installed.
:eval-all     Evaluate all available formulae and casks, whether installed or
              not, to print their JSON. Implied if HOMEBREW_EVAL_ALL is set.
:variations   Include the variations hash in each formula's JSON output.

:debug        Display a trace of all shell commands as they are executed.
:quiet        Make some output more quiet.
:verbose      Print the directories checked and git operations performed."
  (homebrew--run-package-cmd "info" names args '()))

(defun homebrew-package-install (names &rest args)
  "Install a formula or cask in NAMES.

The ARGS plist may contain any of the following property flags where t turns
on the flag and nil turns off the flag.

INCOMPLETE!

:debug        Display a trace of all shell commands as they are executed.
:quiet        Make some output more quiet.
:verbose      Print the directories checked and git operations performed."
  (homebrew--run-package-cmd "install" names args '()))

(defun homebrew-package-reinstall (names &rest args)
  "Reinstall a formula or cask in NAMES.

The ARGS plist may contain any of the following property flags where t turns
on the flag and nil turns off the flag.

INCOMPLETE!

:debug        Display a trace of all shell commands as they are executed.
:quiet        Make some output more quiet.
:verbose      Print the directories checked and git operations performed."
  (homebrew--run-package-cmd "reinstall" names args '()))

(defun homebrew-package-upgrade (names &rest args)
  "Upgrade a formula or cask in NAMES.

The ARGS plist may contain any of the following property flags where t turns
on the flag and nil turns off the flag.

INCOMPLETE!

:debug        Display a trace of all shell commands as they are executed.
:quiet        Make some output more quiet.
:verbose      Print the directories checked and git operations performed."
  (homebrew--run-package-cmd "upgrade" names args '()))

(defun homebrew-package-delete (names &rest args)
  "Uninstall a formula or cask in NAMES.

The ARGS plist may contain any of the following property flags where t turns
on the flag and nil turns off the flag.

:formula      Treat all named arguments as formulae.
:cask         Treat all named arguments as casks.
:force        Delete all installed versions of formula. Uninstall even if
              cask is not installed, overwrite existing files and ignore
              errors when removing files.
:zap          Remove all files associated with a cask. May remove files which
              are shared between applications.
:ignore-dependencies
              Don't fail uninstall, even if formula is a dependency of any
              installed formulae.

:debug        Display a trace of all shell commands as they are executed.
:quiet        Make some output more quiet.
:verbose      Print the directories checked and git operations performed."
  (homebrew--run-package-cmd "uninstall" names args '()))

(defun homebrew-package-list-installed (names &rest args)
  "List all installed formulae or casks in NAMES.

The ARGS plist may contain any of the following property flags where t turns
on the flag and nil turns off the flag.

:debug        Display a trace of all shell commands as they are executed.
:quiet        Make some output more quiet.
:verbose      Print the directories checked and git operations performed."
  (homebrew--run-package-cmd "list" names args '()))

(defun homebrew-package-deps (names &rest args)
  "Show dependencies for formula in NAMES.

Additional options specific to formula may be appended to the command. When
given multiple formula arguments, show the intersection of dependencies for
each formula.

The ARGS plist may contain any of the following property flags where t turns
on the flag and nil turns off the flag.

:formula      Treat all named arguments as formulae.
:cask         Treat all named arguments as casks.
:topological  Sort dependencies in topological order.
:direct       Show only the direct dependencies declared in the formula.
:union        Show the union of dependencies for multiple formula, instead of
              the intersection.
:full-name    List dependencies by their full name.
:include-build
              Include `:build' dependencies for formula.
:include-optional
              Include `:optional' dependencies for formula.
:include-test
              Include `:test' dependencies for formula (non-recursive).
:skip-recommended
              Skip :recommended dependencies for formula.
:include-requirements
              Include requirements in addition to dependencies for formula.
:tree         Show dependencies as a tree. When given multiple formula
              arguments, show individual trees for each formula.
:graph        Show dependencies as a directed graph.
:dot          Show text-based graph description in DOT format.
:annotate     Mark any build, test, optional, or recommended dependencies as
              such in the output.
:installed    List dependencies for formulae that are currently installed.
              If formula is specified, list only its dependencies that are
              currently installed.
:eval-all     Evaluate all available formulae and casks, whether installed or
              not, to list their dependencies.
:for-each     Switch into the mode used by the `:eval-all' option, but only
              list dependencies for each provided formula, one formula per
              line. This is used for debugging the `:installed'/`:eval-all'
              display mode.

:debug        Display a trace of all shell commands as they are executed.
:quiet        Make some output more quiet.
:verbose      Print the directories checked and git operations performed."
  (homebrew--run-package-cmd "deps" names args '()))

(defun homebrew-package-options (name &rest args)
  "Show install options specific to formula in NAME.

The ARGS plist may contain any of the following property flags where t turns
on the flag and nil turns off the flag.

:compact      Show all options on a single line separated by spaces.
:installed    Show options for formulae that are currently installed.
:eval-all     Evaluate all available formulae and casks, whether installed
              or not, to show their options.
:command      Show options for the specified command.

:debug        Display a trace of all shell commands as they are executed.
:quiet        Make some output more quiet.
:verbose      Print the directories checked and git operations performed."
  (homebrew--run-package-cmd "options" (list name) args '()))

(defun homebrew-package-pin (names &rest args)
  "Pin the specified formulae in NAMES.

Pinning formulae prevents them from being upgraded when issuing the brew
upgrade formula command. See also `homebrew-package-unpin'.

The ARGS plist may contain any of the following property flags where t turns
on the flag and nil turns off the flag.

:debug        Display a trace of all shell commands as they are executed.
:quiet        Make some output more quiet.
:verbose      Print the directories checked and git operations performed."
  (homebrew--run-package-cmd "pin" names args '()))

(defun homebrew-package-unpin (names &rest args)
  "Unpin the specified formulae in NAMES.

Pinning formulae prevents them from being upgraded when issuing the brew
upgrade formula command. See also `homebrew-package-pin'.

The ARGS plist may contain any of the following property flags where t turns
on the flag and nil turns off the flag.

:debug        Display a trace of all shell commands as they are executed.
:quiet        Make some output more quiet.
:verbose      Print the directories checked and git operations performed."
  (homebrew--run-package-cmd "pin" names args '()))

(defun homebrew-package-uses (names &rest args)
  "Show formulae and casks that specify NAMES as a dependency.

When given multiple formula arguments, show the
intersection of formulae that use formula. By default, uses shows all
formulae and casks that specify formula as a required or recommended
dependency for their stable builds.

The ARGS plist may contain any of the following property flags where t turns
on the flag and nil turns off the flag.

:formula      Include only formulae.
:cask         Include only casks.
:recursive    Resolve more than one level of dependencies.
:installed    Only list formulae and casks that are currently installed.
:eval-all     Evaluate all available formulae and casks, whether installed
              or not, to show their dependents.
:include-build
              Include all formulae that specify formula as `:build' type
              dependency.
:include-test
              Include all formulae that specify formula as `:test' type
              dependency.
:include-optional
              Include all formulae that specify formula as `:optional' type
              dependency.
:skip-recommended
              Skip all formulae that specify formula as `:recommended' type
              dependency.

:debug        Display a trace of all shell commands as they are executed.
:quiet        Make some output more quiet.
:verbose      Print the directories checked and git operations performed."
  (homebrew--run-package-cmd "uses" names args '()))

(defun homebrew-tap-list (&rest args)
  "List all installed taps.

The ARGS plist may contain any of the following property flags where t turns
on the flag and nil turns off the flag.

:debug        Display a trace of all shell commands as they are executed.
:quiet        Make some output more quiet.
:verbose      Print the directories checked and git operations performed."
  (homebrew--run-package-cmd "tap" names args '()))

(defun homebrew-tap-install (name &rest args)
  "Tap a formula repository.

The specified NAME should be of the form \"{user}/{repo}\".

With `:url' unspecified, tap a formula repository from GitHub using HTTPS.
Since so many taps are hosted on GitHub, the following example:

  (homebrew-tap-install \"user/repo\")

is a shortcut for:

  (homebrew-tap-install \"user/repo\"
                        :url \"https://github.com/{user}/homebrew-{repo}\")

With `:url' specified, tap a formula repository from anywhere, using any
transport protocol that `git(1)' handles. The one-argument form of tap
simplifies but also limits. This two-argument command makes no assumptions, so
taps can be cloned from places other than GitHub and using protocols other
than HTTPS, e.g. SSH, git, HTTP, FTP(S), rsync.

The ARGS plist may contain any of the following property flags where t turns
on the flag and nil turns off the flag.

:url          The location of the `git(1)' repository.
:full         Convert a shallow clone to a full clone without untapping.
              Taps are only cloned as shallow clones if `:shallow' was
              originally passed. (disabled)
:shallow      Fetch tap as a shallow clone rather than a full clone. Useful
              for continuous integration. (disabled)
:force-auto-update
              Auto-update tap even if it is not hosted on GitHub. By
              default, only taps hosted on GitHub are auto-updated (for
              performance reasons).
:custom-remote'
              Install or change a tap with a custom remote. Useful for
              mirrors.
:repair       Migrate tapped formulae from symlink-based to directory-based
              structure.
:list-pinned  List all pinned taps.
:eval-all     Evaluate all the formulae, casks and aliases in the new tap
              to check validity. Implied if `HOMEBREW_EVAL_ALL' is set.

:debug        Display a trace of all shell commands as they are executed.
:quiet        Make some output more quiet.
:verbose      Print the directories checked and git operations performed."
  (homebrew--run-cmd "tap" name args '()))

(defun homebrew-tap-delete (name &rest args)
  "Remove a tapped formula repository.

The provided NAME has to be a tap previously added by `homebrew-tap-install'.

The ARGS plist may contain any of the following property flags where t turns
on the flag and nil turns off the flag.

:force        Untap even if formulae or casks from this tap are currently
              installed.

:debug        Display a trace of all shell commands as they are executed.
:quiet        Make some output more quiet.
:verbose      Print the directories checked and git operations performed."
  (homebrew--run-cmd "untap" name args '()))

(provide 'homebrew)
;;; homebrew.el ends here
