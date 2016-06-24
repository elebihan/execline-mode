`execline-mode`: A major Emacs mode for editing execline scripts
================================================================

`execline-mode` makes editing
[execline](http://skarnet.org/software/execline/index.html) scripts with Emacs
enjoyable. `execline-mode` requires Emacs 24 or later.

## Manual Installation

To install manually, check out this repository and add this to your
`.emacs` file:

```lisp
(add-to-list 'load-path "/path/to/execline-mode/")
(require 'execline-mode)
```
## Usage

This major mode will be autoloaded whenever an execline script is visited. This
includes files with `execlineb` in the shebang or where the local variable
"mode" is set to "execline".

## License

`execline-mode` is distributed under the terms of the GNU General Public License
(Version 3.0).

See [LICENSE](LICENSE) for details.
