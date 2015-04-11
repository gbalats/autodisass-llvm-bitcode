[![License GPL 3][badge-license]](http://www.gnu.org/licenses/gpl-3.0.txt)
[![MELPA][melpa-badge]][melpa-package]
[![MELPA Stable][melpa-stable-badge]][melpa-stable-package]

autodisass-llvm-bitcode
=======================

This package enables the automatic disassembly of LLVM bitcode inside
Emacs buffers, when opening LLVM bitcode (.bc) files.

When `llvm-mode` is available, it is automatically selected for the
current LLVM bitcode-containing buffer. You can download `llvm-mode`
from [here](http://llvm.org/svn/llvm-project/llvm/trunk/utils/emacs/llvm-mode.el).

In any case, `llvm-dis` must be installed in the system for this
extension to have any effect, since that is the tool that actually
performs the disassembly.


## Installation

You can install this package using the `package.el` built-in package
manager in Emacs. It is available on [MELPA](http://melpa.org/#/) and
[MELPA Stable](http://stable.melpa.org/#/) repos.

If you have these enabled, simply run:

    M-x package-install [RET] autodisass-llvm-bitcode [RET]


Alternatively, you can save
[this .el file](autodisass-llvm-bitcode.el) to a directory in your
*load-path*, and add the following to your `.emacs`:

    (require 'autodisass-llvm-bitcode)

[badge-license]: https://img.shields.io/badge/license-GPL_3-green.svg
[melpa-badge]: http://melpa.org/packages/autodisass-llvm-bitcode-badge.svg
[melpa-stable-badge]: http://stable.melpa.org/packages/autodisass-llvm-bitcode-badge.svg
[melpa-package]: http://melpa.org/#/autodisass-llvm-bitcode
[melpa-stable-package]: http://stable.melpa.org/#/autodisass-llvm-bitcode
