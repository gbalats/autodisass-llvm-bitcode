autodisass-llvm-bitcode
=======================

This package enables the automatic disassembly of LLVM bitcode inside
Emacs buffers, when opening LLVM bitcode (.bc) files.

When `llvm-mode` is available, it is automatically selected for the
current LLVM bitcode-containing buffer.

In any case, `llvm-dis` must be installed in the system for this
extension to have any effect, since that is the tool that actually
performs the disassembly.


To use, save [this .el file](autodisass-llvm-bitcode.el) to a
directory in your *load-path*, and add the following to your `.emacs`:

    (require 'autodisass-llvm-bitcode)
