ZOWIE version history
=====================

Version 1.0 (Dec 29 2009)
-------------------------

*   Initial release.

Version 1.0 revision 2011.1214
------------------------------

*   Trivial tweaks to the documentation's HTML.

Version 1.0 revision 2012.0325
------------------------------

*   Converted documentation to Markdown format.
*   PEP-8 cleanups to `zowie.py`; no functional changes.

Version 1.0 revision 2014.0819
------------------------------

*   Added ability to run `zowie.py` under Skulpt, and demo HTML page of this.
*   Added some rudimentary Falderal tests.
*   Added UNLICENSE to emphasize the public domain status of these materials.
*   Made `zowie.py` file executable, using `/usr/bin/env` to find `python`.
*   More Markdown, whitespace, and PEP-8 cleanups.

Version 1.1
-----------

*   Made syntax more strict:
    *   Names of commands and registers must be in uppercase.
    *   Previous lax parsing of indirect register references was dropped.
*   Because this above changed the definition of the language, but not in any
    jaw-dropping way, the minor version number was bumped.
*   Uses of regular expressions and old-style classes `zowie.py` were dropped.
*   Added ability to compile `zowie.py` with RPython from PyPy version 2.3.1.

Version 1.1 revision 2019.0122
------------------------------

*   Added example Javascript files demonstrating how zowie.py can be run
    under Skulpt in a web browser.

Version 1.1 revision 2021.0622
------------------------------

*   Updated reference implementation to be able to run under either Python 2
    or Python 3.
*   Allowed reference implementation to be compiled by the RPython compiler
    from PyPy version 7.3.5.  Note that the RPython-compiled version cannot
    currently output Unicode, and always falls back to the `&#...;` syntax
    when outputting non-ASCII characters.
*   More idiomatic structuring of the Falderal test files.

Version 1.1 revision 2021.0729
------------------------------

*   Fixed typo in spec spotted by [Sgeo](https://github.com/Sgeo).
*   Added an implementation in Haskell.
*   Added a few more tests cases to test suite.
*   Fixed some bugs in __str__ methods in the Python implementation (that are not normally used).
*   Fixed and modernized HTML5 installation.
