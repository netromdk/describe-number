# describe-number

[![MELPA](http://melpa.org/packages/describe-number-badge.svg)](http://melpa.org/#/describe-number)

Describe number value at point or region in Emacs. If value is a number then binary/octal/decimal/hexadecimal/character values and conversions are shown. For strings each character is processed in the same way.

Use `describe-number-at-point` on point/region or `describe-number` to input value manually.

Might be preferable to bind `describe-number-at-point` to some key:
```elisp
(global-set-key (kbd "C-c ?") 'describe-number-at-point)
```

# Demo
When using "101" as input to `describe-number-at-point` or `describe-number`:
![](demo-num.png)

In the case of using a string, e.g. "bin", it will process each character value separately:
![](demo-string.png)

"b->d" means binary to decimal, "o->d" means octal to decimal, and "x->d" means hexadecimal to decimal.
