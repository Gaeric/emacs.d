;;; wesl-ts-mode.el --- Tree-sitter support for the WebGPU Shading Language -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Anthony Cowley
;; Author: Anthony Cowley
;;         Gaeric
;; Package-Requires: ((emacs "29.1"))
;; Keywords: wesl tree-sitter
;; Version: 1.0

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Syntax highlighting for the WebGPU Shading Language (WGSL) with support for WESL extensions
;; based on a tree-sitter grammar.
;;
;; The approach taken here is based on `rust-ts-mode' by Randy Taylor.

;;; Code:

(require 'prog-mode)
(require 'treesit)
(require 'c-ts-common)

(defvar wesl-ts-mode--operators
  '("!"  "!=" "%" "%=" "&" "&=" "&&" "*" "*=" "+" "+=" "," "-" "-="
    "->" "." "/" "/=" ":" ";" "<<" "<" "<="
    "=" "==" ">" ">=" ">>" "@" "^" "^=" "|" "|=" "||")
  "WGSL operators for tree-sitter font-locking.")

(defvar wesl-ts-mode--keywords
  '("if" "else" "fn" "switch" "case" "break" "default" "loop"
    "continue" "continuing" "for" "let" "var" "return" "struct"
    "type" "while")
  "WGSL keywords for tree-sitter font-locking.")

(defvar wesl-ts-mode--builtins
  '(;; Constructor built-in functions
    ;"array" "bool" "f16" "f32" "i32" "mat2x2" "mat2x3" "mat2x4" "mat3x3"
    ;"mat3x3" "mat3x4" "mat4x2" "mat4x3" "mat4x4" "u32" "vec2" "vec3" "vec4"
    ;; Bit reinterpretation built-in functions
    "bitcast"
    ;; Logical built-in functions
    "all" "any" "select"
    ;; Array built-in functions
    "arrayLength"
    ;; Numeric built-in functions
    "abs" "acos" "acosh" "asin" "asinh" "atan" "atanh" "atan2" "ceil" "clamp"
    "cos" "cosh" "countLeadingZeros" "countOneBits" "countTrailingZeros"
    "cross" "degrees" "determinant" "distance" "dot" "exp" "exp2"
    "extractBits" "faceForward" "firstLeadingBit" "firstTrailingBit"
    "floor" "fma" "fract" "frexp" "insertBits" "inverseSqrt" "ldexp"
    "length" "log" "log2" "max" "min" "mix" "modf" "normalize" "pow"
    "quantizeToF16" "radians" "reflect" "refract" "reverseBits" "round"
    "saturate" "sign" "sin" "sinh" "smoothstep" "sqrt" "step" "tan" "tanh"
    "transpose" "trunc"
    ;; Derivative built-in functions
    "dpdx" "dpdxCoarse" "dpdxFine" "dpdy" "dpdyCoarse" "dpdyFine" "fwidth"
    "fwidthCoarse" "fwidthFine"
    ;; Texture built-in functions
    "textureDimensions" "textureGather" "textureGatherCompare" "textureLoad"
    "textureNumLayers" "textureNumLevels" "textureNumSamples" "textureSample"
    "textureSampleBias" "textureSampleCompare" "textureSampleCompareLevel"
    "textureSampleGrad" "textureSampleLevel" "textureSampleBaseClampToEdge"
    "textureStore"
    ;; Data packing built-in functions
    "pack4x8snorm" "pack4x8unorm" "pack2x16snorm" "pack2x16unorm"
    "pack2x16float"
    ;; Data unpacking built-in functions
    "unpack4x8snorm" "unpack4x8unorm" "unpack2x16snorm" "unpack2x16unorm"
    "unpack2x16float"
    ;; Synchronization built-in functions
    "storageBarrier" "textureBarrier" "workgroupBarrier" "workgroupUniformLoad"
    ;; Built-in inputs and outputs
    "frag_depth" "front_facing" "global_invocation_id" "instance_index"
    "local_invocation_id" "local_invocation_index" "num_workgroups"
    "position" "sample_index" "sample_mask" "vertex_index" "workgroup_id"
    )
  "WGSL built-in functions from https://www.w3.org/TR/WGSL/")

;; Note: The built-in inputs and outputs should perhaps not be lumped
;; in with the other built-in keywords. They are used in attributes,
;; but classifying them as general built-ins means that any use of
;; these identifiers receives the syntax highlighting of a built-in
;; value rather than a regular identifier.

(setq wesl-ts-mode--builtins-hash-table
      (let ((tbl (make-hash-table :test 'equal)))
        (mapc (lambda (x) (puthash x t tbl)) wesl-ts-mode--builtins)
        tbl))

(defun wesl-ts-mode--is-builtin? (x)
  (gethash (treesit-node-text x) wesl-ts-mode--builtins-hash-table))

(defvar wesl-ts-mode--font-lock-rules
  `(:language wesl
    :override t
    :feature comment
    (([(line_comment) (block_comment)]) @font-lock-comment-face)


    ;; import xx as xx
   :language wesl
   :override t
   :feature definition
   ((import_item "as" @font-lock-comment-face)
    (import_statement "import" @font-lock-comment-face))

   :language wesl
   :override t
   :feature string
   ((import_item name: (identifier) @font-lock-variable-name-face)
    (import_item rename: (identifier) @font-lock-variable-name-face))

   :language wesl
   :override t
   :feature keyword
   ((struct_decl "struct" @font-lock-keyword-face)
    (attribute "@" @font-lock-preprocessor-face))


   :language wesl
   :override t
   :feature type
   ((struct_decl name: (identifier) @font-lock-type-face)
    (struct_member type: (type_specifier (identifier) @font-lock-type-face))
    (type_specifier (template_list (identifier) @font-lock-type-face)))

   :language wesl
   :override t
   :feature definition
   ((struct_member name: (identifier) @font-lock-variable-name-face)
    (attribute name: (identifier) @font-lock-function-call-face))


   :language wesl
   :override t
   :feature number
   (([(int_literal) (float_literal)]) @font-lock-number-face)

   :language wesl
   :override t
   :feature keyword
   ((variable_decl "var" @font-lock-keyword-face))

   :language wesl
   :override t
   :feature definition
   ((variable_decl name: (identifier) @font-lock-variable-name-face))

   :language wesl
   :override t
   :feature type
   ((variable_decl type: (type_specifier (identifier)) @font-lock-type-face)
    (variable_decl (template_list (identifier) @font-lock-type-face)))

))

(defcustom wesl-ts-mode-indent-offset 2
  "Number of spaces for each indentation step in `wesl-ts-mode'."
  :version "29.1"
  :type 'integer
  :safe 'integerp
  :group 'wesl)

(defvar wesl-ts-mode--indent-rules
  `((wesl
     ((parent-is "source_file") column-0 0)
     ((node-is ")") parent-bol 0)
     ((node-is "]") parent-bol 0)
     ((node-is "}") (and parent parent-bol) 0)
     ((and (parent-is "comment") c-ts-common-looking-at-star)
      c-ts-common-comment-start-after-first-star -1)
     ((parent-is "comment") prev-adaptive-prefix 0)
     ((parent-is "arguments") parent-bol wesl-ts-mode-indent-offset)
     ((parent-is "assignment_statement") parent-bol wesl-ts-mode-indent-offset)
     ((parent-is "array_expression") parent-bol wesl-ts-mode-indent-offset)
     ((parent-is "binary_expression") parent-bol wesl-ts-mode-indent-offset)
     ((parent-is "compound_statement") parent-bol wesl-ts-mode-indent-offset)
     ((parent-is "declaration_list") parent-bol wesl-ts-mode-indent-offset)
     ((parent-is "enum_variant_list") parent-bol wesl-ts-mode-indent-offset)
     ((parent-is "field_declaration_list") parent-bol wesl-ts-mode-indent-offset)
     ((parent-is "field_expression") parent-bol wesl-ts-mode-indent-offset)
     ((parent-is "field_initializer_list") parent-bol wesl-ts-mode-indent-offset)
     ((parent-is "let_declaration") parent-bol wesl-ts-mode-indent-offset)
     ((parent-is "var_declaration") parent-bol wesl-ts-mode-indent-offset)
     ((parent-is "parameters") parent-bol wesl-ts-mode-indent-offset)
     ((parent-is "struct_pattern") parent-bol wesl-ts-mode-indent-offset)))
  "Tree-sitter indent rules for `wesl-ts-mode'.")

(defun wesl-ts-mode--syntax-propertize (beg end)
  "Apply syntax properties to special characters between BEG and END.

Apply syntax properties to various special characters with
contextual meaning between BEG and END.

The apostrophe \\=' should be treated as string when used for char literals.

< and > are usually punctuation, e.g., as greater/less-than.  But
when used for types, they should be considered pairs.

This function checks for < and > in the changed RANGES and apply
appropriate text property to alter the syntax of template
delimiters < and >'s."
  (goto-char beg)
  (while (search-forward "'" end t)
    (when (string-equal "char_literal"
                        (treesit-node-type
                         (treesit-node-at (match-beginning 0))))
      (put-text-property (match-beginning 0) (match-end 0)
                         'syntax-table (string-to-syntax "\""))))
  (goto-char beg)
  (while (re-search-forward (rx (or "<" ">")) end t)
    (pcase (treesit-node-type
            (treesit-node-parent
             (treesit-node-at (match-beginning 0))))
      (;(or "type_declaration" "type_parameters")
       "type_declaration"
       (put-text-property (match-beginning 0)
                          (match-end 0)
                          'syntax-table
                          (pcase (char-before)
                            (?< '(4 . ?>))
                            (?> '(5 . ?<))))))))

(defun wesl-ts-mode--defun-name (node)
  "Return the defun name of NODE.
Return nil if there is no name or if NODE is not a defun node."
  (pcase (treesit-node-type node)
    ("function_declaration"
     (treesit-node-text
      (treesit-node-child-by-field-name node "name") t))
    ("struct_declaration"
     (treesit-node-text
      (treesit-node-child-by-field-name node "name") t))
    ("type_declaration"
     (treesit-node-text
      (treesit-node-child-by-field-name node "name") t))))

(defvar wesl-ts-mode--syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?+   "."      table)
    (modify-syntax-entry ?-   "."      table)
    (modify-syntax-entry ?=   "."      table)
    (modify-syntax-entry ?%   "."      table)
    (modify-syntax-entry ?&   "."      table)
    (modify-syntax-entry ?|   "."      table)
    (modify-syntax-entry ?^   "."      table)
    (modify-syntax-entry ?!   "."      table)
    (modify-syntax-entry ?@   "."      table)
    (modify-syntax-entry ?~   "."      table)
    (modify-syntax-entry ?<   "."      table)
    (modify-syntax-entry ?>   "."      table)
    (modify-syntax-entry ?/   ". 124b" table)
    (modify-syntax-entry ?*   ". 23"   table)
    (modify-syntax-entry ?\n  "> b"    table)
    (modify-syntax-entry ?\^m "> b"    table)
    table)
  "Syntax table for `wesl-ts-mode'.")

(defun wesl-ts-setup ()
  "Setup tree-sitter for wesl-ts-mode."
  (setq-local syntax-propertize-function
              #'wesl-ts-mode--syntax-propertize)
  (c-ts-common-comment-setup)

  (setq-local treesit-font-lock-settings
              (apply #'treesit-font-lock-rules
                     wesl-ts-mode--font-lock-rules))

  (setq-local treesit-font-lock-feature-list
              '((comment definition)
                (keyword string)
                (assignment attribute builtin constant escape-sequence number
                            type address_space texel_format bitcast funcall)
                (bracket delimiter error function operator property variable)))

  (setq-local treesit-simple-imenu-settings
              `(("Struct" "\\`struct_declaration\\'" nil nil)
                ("Fn" "\\`function_declaration\\'" nil nil)))

  (setq-local treesit-font-lock-level 4)
  (setq-local indent-tabs-mode nil
              treesit-simple-indent-rules wesl-ts-mode--indent-rules)
  (setq-local treesit-defun-type-regexp
              (regexp-opt '("function_declaration"
                            "struct_declaration")))

  (setq-local treesit-defun-name-function #'wesl-ts-mode--defun-name)
  (treesit-major-mode-setup))

;;;###autoload
(define-derived-mode wesl-ts-mode prog-mode "WESL[ts]"
  "Major mode for editing WESL with tree-sitter."
  :syntax-table wesl-ts-mode--syntax-table
  (when (treesit-ready-p 'wesl)
    (treesit-parser-create 'wesl)
    (wesl-ts-setup)))

(if (treesit-ready-p 'wesl)
    (add-to-list 'auto-mode-alist '("\\.wesl\\'" . wesl-ts-mode)))

(provide 'wesl-ts-mode)

;;; wesl-ts-mode.el ends here
